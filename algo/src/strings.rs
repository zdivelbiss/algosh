use intaglio::{Symbol, SymbolTable};
use parking_lot::RwLock;

lazy_static::lazy_static! {
    static ref STRING_CACHE: RwLock<SymbolTable> = RwLock::new(SymbolTable::new());
}

pub fn intern_str(string: &str) -> Symbol {
    let cache = STRING_CACHE.upgradable_read();
    cache.check_interned(string).unwrap_or_else(|| {
        let mut cache = parking_lot::RwLockUpgradableReadGuard::upgrade(cache);
        cache
            .intern(string.to_string())
            .expect("symbol overflow in string cache")
    })
}

pub fn get_intern_str(symbol: Symbol) -> String {
    let cache = STRING_CACHE.read();
    cache
        .get(symbol)
        .expect("unknown symbol for symbol table")
        .to_string() // FIXME: Don't allocate a new string here
}
