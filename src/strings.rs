use std::sync::LazyLock;

use intaglio::Symbol;
use parking_lot::RwLock;

static STRING_CACHE: LazyLock<RwLock<intaglio::SymbolTable>> =
    LazyLock::new(|| RwLock::new(intaglio::SymbolTable::new()));

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
    cache.get(symbol).expect("unknown symbol for symbol table").to_string()
}
