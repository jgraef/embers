

//#[derive(ComputeShader)]
//pub struct Map<A, B, R, ;

#[ricsl::kernel]
fn map<T: Element, F: RicslClosure<T, T>>(
    #[binding]
    a: &Tensor<T>,
    #[param]
    f: F,
) -> Tensor<T> {
    f(a)
}