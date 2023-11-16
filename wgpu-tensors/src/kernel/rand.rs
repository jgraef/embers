use std::marker::PhantomData;

use askama::Template;
use rand::{
    thread_rng,
    Rng,
    SeedableRng,
};
use rand_xorshift::XorShiftRng;

use super::{
    binding::{
        KernelBindingBuilder,
        KernelBindingDeclaration,
        KernelDeclaration,
        KernelParameterDeclaration,
    },
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
    TaskPartition,
};
use crate::{
    element::{
        Element,
        Encode,
    },
    error::KernelError,
    tensor::strider::contiguous_strides,
    Gpu,
    Tensor,
};

pub trait Distribution<T>: 'static {
    const PREPARE: &'static str;
    const SAMPLE: &'static str;
    const INDEX_STEP: usize = 1;
    const SAMPLE_ENCODED: bool = false;

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError>;
}

#[derive(Debug, Default)]
pub struct Standard;

#[derive(Debug)]
pub struct Uniform<T> {
    pub min: T,
    pub max: T,
}

pub struct Normal<T> {
    pub mu: T,
    pub sigma: T,
}

impl Default for Normal<f32> {
    fn default() -> Self {
        Self { mu: 0., sigma: 1. }
    }
}

impl Distribution<u32> for Standard {
    const PREPARE: &'static str = "";
    const SAMPLE: &'static str = concat!("let sampled = prng_generate_u32(&prng);");
    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", 0)?;
        builder.add_parameter("dist_bias", 0)?;
        Ok(())
    }
}

impl Distribution<i32> for Standard {
    const PREPARE: &'static str = "";

    const SAMPLE: &'static str = concat!("let sampled = prng_generate_i32(&prng);");

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", 0)?;
        builder.add_parameter("dist_bias", 0)?;
        Ok(())
    }
}

impl Distribution<bool> for Standard {
    const PREPARE: &'static str = "";

    const SAMPLE: &'static str = concat!("let sampled = prng_generate_u32(&prng);");

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", 0)?;
        builder.add_parameter("dist_bias", 0)?;
        Ok(())
    }

    const INDEX_STEP: usize = <bool as Encode>::NUM_PACKED;

    const SAMPLE_ENCODED: bool = true;
}

impl Distribution<u32> for Uniform<u32> {
    const PREPARE: &'static str = r#"
    let dist_scale = u32(p_dist_scale());
    let dist_bias = u32(p_dist_bias());
    "#;

    const SAMPLE: &'static str = "let sampled = prng_generate_u32(&prng) % dist_scale + dist_bias;";

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", self.max - self.min)?;
        builder.add_parameter("dist_bias", self.min)?;
        Ok(())
    }
}

impl Distribution<i32> for Uniform<i32> {
    const PREPARE: &'static str = r#"
    let dist_scale = i32(p_dist_scale());
    let dist_bias = i32(p_dist_bias());
    "#;

    const SAMPLE: &'static str = "let sampled = prng_generate_i32(&prng) % dist_scale + dist_bias;";

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", self.max - self.min)?;
        builder.add_parameter("dist_bias", self.min)?;
        Ok(())
    }
}

impl Distribution<f32> for Uniform<f32> {
    const PREPARE: &'static str = r#"
    let dist_scale = bitcast<f32>(p_dist_scale());
    let dist_bias = bitcast<f32>(p_dist_bias());
    "#;

    const SAMPLE: &'static str =
        "let sampled = prng_generate_f32_uniform(&prng) * dist_scale + dist_bias;";

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", (self.max - self.min).to_bits())?;
        builder.add_parameter("dist_bias", self.min.to_bits())?;
        Ok(())
    }
}

impl Distribution<f32> for Normal<f32> {
    const PREPARE: &'static str = r#"
    let dist_sigma = bitcast<f32>(p_dist_scale());
    let dist_mu = bitcast<f32>(p_dist_bias());
    "#;

    const SAMPLE: &'static str =
        "let sampled = prng_generate_f32_normal(&prng) * dist_sigma + dist_mu;";

    fn add_parameters<'gpu, 'tensor, const D: usize>(
        &self,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_parameter("dist_scale", self.sigma.to_bits())?;
        builder.add_parameter("dist_bias", self.mu.to_bits())?;
        Ok(())
    }
}

pub struct RandKernel<R, U>(PhantomData<(R, U)>);

impl<R: Element, U: Distribution<R>> Kernel for RandKernel<R, U> {
    const LABEL: &'static str = "Rand";

    type Template = RandKernelTemplate;

    type Signature = RandSignature<R, U>;

    fn template(info: KernelTemplateInfo) -> Self::Template {
        RandKernelTemplate {
            info,
            prepare: U::PREPARE,
            sample: U::SAMPLE,
            index_step: U::INDEX_STEP,
            sample_encoded: U::SAMPLE_ENCODED,
        }
    }
}

pub struct RandArgs<'a, const D: usize, R: Element, U: Distribution<R>> {
    output: &'a Tensor<D, R>,
    seed: [u32; 4],
    distribution: U,
}

pub struct RandSignature<R, U>(PhantomData<(R, U)>);

impl<R: Element, U: Distribution<R>> KernelSignature for RandSignature<R, U> {
    const DECLARATION: KernelDeclaration = KernelDeclaration {
        bindings: &[KernelBindingDeclaration::read_write::<R>("output")],
        parameters: &[
            KernelParameterDeclaration::array("op_strides"),
            KernelParameterDeclaration::array("op_shape"),
            KernelParameterDeclaration::int("output_offset"),
            KernelParameterDeclaration::array("output_strides"),
            KernelParameterDeclaration::array("seed"),
            KernelParameterDeclaration::int("dist_scale"),
            KernelParameterDeclaration::int("dist_bias"),
        ],
    };

    type Args<'a, const D: usize> = RandArgs<'a, D, R, U>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_binding("output", &args.output)?;

        let output_strider = args.output.strider();
        let op_shape = output_strider.shape();
        builder.add_parameter("op_strides", contiguous_strides(&op_shape))?;
        builder.add_parameter("op_shape", op_shape)?;

        builder.add_parameter("output_offset", output_strider.offset())?;
        builder.add_parameter("output_strides", output_strider.strides())?;

        builder.add_parameter("seed", args.seed)?;
        args.distribution.add_parameters(builder)?;
        Ok(())
    }

    fn task_partition<'a, const D: usize>(args: &Self::Args<'a, D>) -> TaskPartition {
        TaskPartition::for_result(&args.output)
    }
}

#[derive(Debug, Template)]
#[template(path = "rand.wgsl")]
pub struct RandKernelTemplate {
    info: KernelTemplateInfo,
    prepare: &'static str,
    sample: &'static str,
    sample_encoded: bool,
    index_step: usize,
}

impl Gpu {
    pub async fn rand<const D: usize, T: Element, U: Distribution<T>>(
        &self,
        shape: [usize; D],
        distribution: U,
        seed: Option<Seed>,
    ) -> Result<Tensor<D, T>, KernelError> {
        let result = Tensor::allocate(self, shape);
        self.run_kernel::<D, RandKernel<T, U>>(RandArgs {
            output: &result,
            seed: generate_seed(seed),
            distribution,
        })
        .await?;
        Ok(result)
    }
}

pub type Seed = [u8; 16];

fn generate_seed(seed: Option<Seed>) -> [u32; 4] {
    // seed an rng from the provided seed or a random number.
    let seed = seed.unwrap_or_else(|| thread_rng().gen());
    let mut rng = XorShiftRng::from_seed(seed);

    // sample a seed from the CPU PRNG.
    let mut seed = [0; 4];
    for s in seed.iter_mut() {
        *s = rng.gen::<u32>();
    }

    seed
}
