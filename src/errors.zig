pub const parseError = error{
    Syntax,
};
pub const evalError = error{
    SymbolNotFound,
    PlusNonInt,
};
