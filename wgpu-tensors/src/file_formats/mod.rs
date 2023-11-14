use std::io::Error;

use byteorder::ByteOrder;
use futures_lite::io::AsyncReadExt;

pub mod gguf;

macro_rules! byteorder_read {
    ($method:ident, $ty:ident, $num_bytes:expr) => {
        async fn $method<T: ByteOrder>(&mut self) -> Result<$ty, Error> {
            let mut buf = [0; 1];
            self.read_exact(&mut buf).await?;
            Ok(T::$method(&buf))
        }
    };
}

trait ReadBytesAsyncExt: AsyncReadExt + Unpin {
    async fn read_u8(&mut self) -> Result<u8, Error> {
        let mut buf = [0; 1];
        self.read_exact(&mut buf).await?;
        Ok(buf[0])
    }

    async fn read_i8(&mut self) -> Result<i8, Error> {
        let mut buf = [0; 1];
        self.read_exact(&mut buf).await?;
        Ok(buf[0] as i8)
    }

    byteorder_read!(read_u16, u16, 2);
    byteorder_read!(read_i16, i16, 2);
    byteorder_read!(read_u32, u32, 4);
    byteorder_read!(read_i32, i32, 4);
    byteorder_read!(read_u64, u64, 8);
    byteorder_read!(read_i64, i64, 8);
    byteorder_read!(read_f32, f32, 4);
    byteorder_read!(read_f64, f64, 8);
}

impl<T: AsyncReadExt + Unpin> ReadBytesAsyncExt for T {}
