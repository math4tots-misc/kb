use super::parse;
use super::BasicError;
use super::File;
use super::RcStr;
use super::Source;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
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

    pub fn find_source(&mut self, module_name: &RcStr) -> Result<Option<&Rc<Source>>, BasicError> {
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

    pub fn list_child_modules(&mut self, module_name: &RcStr) -> Result<Vec<RcStr>, BasicError> {
        let mut module_names = HashSet::new();
        let prefix: RcStr = format!("{}.", module_name).into();
        for (name, _) in &self.map {
            if name == module_name || name.starts_with(prefix.as_ref()) {
                module_names.insert(module_name.clone());
            }
        }
        let relpath =
            PathBuf::from(module_name.replace('.', &format!("{}", std::path::MAIN_SEPARATOR)));
        for source_root in &self.source_roots {
            let start_dir = source_root.join(&relpath);
            module_names.extend(walk_src(&start_dir, &module_name));
        }

        let mut ret: Vec<_> = module_names.into_iter().collect();
        ret.sort();
        Ok(ret)
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
                        message: format!("Module {} not found", name),
                        help: None,
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

/// walks the directory returning all files it finds on a best effort
/// basis (ignores any folders/files it can't opene)
fn walk(start: &Path, extension: Option<&str>) -> Vec<PathBuf> {
    let mut ret = Vec::new();
    let mut stack = vec![start.to_owned()];
    if let Some(ext) = extension {
        if let Some(start) = start.to_str() {
            stack.push(format!("{}{}", start, ext).into());
        }
    }
    while let Some(path) = stack.pop() {
        if path.is_file()
            && extension
                .map(|ext| {
                    path.file_name()
                        .and_then(|p| p.to_str().map(|p| p.ends_with(ext)))
                        .unwrap_or(false)
                })
                .unwrap_or(true)
        {
            ret.push(path);
        } else if path.is_dir() {
            if let Ok(entries) = path.read_dir() {
                for entry in entries {
                    if let Ok(entry) = entry {
                        stack.push(entry.path());
                    }
                }
            }
        }
    }
    ret
}

fn walk_src(start: &Path, prefix: &str) -> Vec<RcStr> {
    let startstr = start.to_str().unwrap();
    let mut module_names = Vec::new();
    for path in walk(start, Some(".kb")) {
        if let Some(pathstr) = path.to_str() {
            let pathstr = pathstr.strip_suffix(".kb").unwrap();
            if let Some(pathstr) = pathstr.strip_prefix(startstr) {
                if pathstr.is_empty() {
                    module_names.push(prefix.into());
                } else {
                    let relname = pathstr.replace(std::path::MAIN_SEPARATOR, ".");
                    let relname = relname.strip_prefix(".").unwrap_or(&relname);
                    let module_name = if prefix.is_empty() {
                        relname.to_owned()
                    } else {
                        format!("{}.{}", prefix, relname)
                    };
                    module_names.push(module_name.into());
                }
            }
        }
    }
    module_names
}
