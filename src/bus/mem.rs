use std::sync::RwLock;

/// A trait that hides access to a memory array that is either
/// thread-locked or bare.
pub trait RamAccess {
    fn with_locked_mem<F, R>(&mut self, f: F) -> R where F: FnMut(&mut [u32]) -> R;
    fn read_word(&self, index: usize) -> u32;
    fn write_word(&mut self, index: usize, word: u32);
    fn read_range(&self, start: usize, length: usize) -> Vec<u32>;
    fn write_range(&mut self, start: usize, content: &[u32]);
}

impl<'a> RamAccess for &'a mut [u32] {
    fn with_locked_mem<F, R>(&mut self, mut f: F) -> R where F: FnMut(&mut [u32]) -> R {
        f(self)
    }
    fn read_word(&self, index: usize) -> u32 {
        self[index]
    }
    fn write_word(&mut self, index: usize, word: u32) {
        self[index] = word
    }
    fn read_range(&self, start: usize, length: usize) -> Vec<u32> {
        self[start..start+length].to_vec()
    }
    fn write_range(&mut self, start: usize, content: &[u32]) {
        for i in 0..content.len() {
            self[start + i] = content[i];
        }
    }
}

impl<'a> RamAccess for &'a RwLock<Box<[u32]>> {
    fn with_locked_mem<F, R>(&mut self, mut f: F) -> R where F: FnMut(&mut [u32]) -> R {
        let mut guard = self.write().unwrap();
        f(&mut guard)
    }
    fn read_word(&self, index: usize) -> u32 {
        self.read().unwrap()[index]
    }
    fn write_word(&mut self, index: usize, word: u32) {
        self.write().unwrap()[index] = word
    }
    fn read_range(&self, start: usize, length: usize) -> Vec<u32> {
        self.read().unwrap()[start..start+length].to_vec()
    }
    fn write_range(&mut self, start: usize, content: &[u32]) {
        let mut w_self = self.write().unwrap();
        for i in 0..content.len() {
            w_self[start + i] = content[i];
        }
    }
}
