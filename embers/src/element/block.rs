use bytemuck::Pod;

use super::wgsl::WgslType;

pub trait Block: Pod + WgslType {
    const NUM_PACKED: usize;

    fn encoded_size(num_elements: usize) -> usize {
        num_elements.div_ceil(Self::NUM_PACKED)
    }

    fn buffer_index(index: usize) -> (usize, usize) {
        (index / Self::NUM_PACKED, index % Self::NUM_PACKED)
    }
}

/*pub trait EncodeBuffer<T: Encode>: Default {
    fn write(&mut self, value: T) -> Option<T::Encoded>;
    fn flush(&mut self) -> Option<T::Encoded>;
}*/

pub trait EncodeIntoBlock<B: Block> {
    //type Buffer: EncodeBuffer<Self>;
    fn encode_into(&self, block: &mut B, i: usize);
}

pub trait DecodeFromBlock<B: Block> {
    fn decode_from(block: &B, i: usize) -> Self;
}
