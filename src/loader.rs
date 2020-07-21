use super::parse;
use super::BasicError;
use super::File;
use super::Source;
use super::RcStr;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::rc::Rc;

pub struct Loader {
    source_roots: Vec<PathBuf>,
    map: HashMap<RcStr, Rc<Source>>,
}

impl Loader {
    pub fn new() -> Self {
        Self {
            source_roots: vec![],
            map: HashMap::new(),
        }
    }

    pub fn add_source(&mut self, source: Rc<Source>) {
        self.map.insert(source.name.clone(), source);
    }

    pub fn add_source_root<P: Into<PathBuf>>(&mut self, path: P) {
        self.source_roots.push(path.into());
    }

    pub fn find_source(
        &mut self,
        module_name: &RcStr,
    ) -> Result<Option<&Rc<Source>>, BasicError> {
        if !self.map.contains_key(module_name) {
            let mut relpath = PathBuf::new();
            let parts: Vec<_> = module_name.split(".").collect();
            for part in &parts[..parts.len() - 1] {
                relpath.push(part);
            }
            if let Some(part) = parts.last() {
                relpath.push(format!("{}.kb", part));
            }
            for root in &self.source_roots {
                let path = root.join(&relpath);
                if path.is_file() {
                    let data = std::fs::read_to_string(path)?;
                    self.map.insert(
                        module_name.clone(),
                        Rc::new(Source {
                            name: module_name.clone(),
                            data: data.into(),
                        }),
                    );
                    break;
                }
            }
        }
        Ok(self.map.get(module_name))
    }

    pub fn load(&mut self, module_name: &RcStr) -> Result<Vec<File>, BasicError> {
        let mut files = Vec::new();
        let mut stack = vec![module_name.clone()];
        let mut seen: HashSet<RcStr> = stack.clone().into_iter().collect();
        while let Some(name) = stack.pop() {
            let source = match self.find_source(&name)? {
                Some(source) => source,
                None => {
                    return Err(BasicError {
                        marks: vec![],
                        message: format!("Module {} not found", module_name),
                    })
                }
            };
            let file = parse(&source)?;
            for imp in &file.imports {
                if !seen.contains(&imp.module_name) {
                    seen.insert(imp.module_name.clone());
                    stack.push(imp.module_name.clone());
                }
            }
            files.push(file);
        }
        files.reverse();
        Ok(files)
    }
}
