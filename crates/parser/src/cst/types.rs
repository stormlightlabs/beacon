use crate::{AstNode, ExceptHandler};

pub(crate) struct InfoIf(
    pub(crate) AstNode,
    pub(crate) Vec<AstNode>,
    pub(crate) Vec<(AstNode, Vec<AstNode>)>,
    pub(crate) Option<Vec<AstNode>>,
);

pub(crate) struct InfoTry(
    pub(crate) Vec<AstNode>,
    pub(crate) Vec<ExceptHandler>,
    pub(crate) Option<Vec<AstNode>>,
    pub(crate) Option<Vec<AstNode>>,
);

pub(crate) struct InfoFor(
    pub(crate) AstNode,
    pub(crate) AstNode,
    pub(crate) Vec<AstNode>,
    pub(crate) Option<Vec<AstNode>>,
    pub(crate) bool,
);

pub(crate) struct InfoArgsKwargs(pub(crate) Vec<AstNode>, pub(crate) Vec<(String, AstNode)>);
