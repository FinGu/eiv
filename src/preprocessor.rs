use std::{collections::HashMap, fmt::Display};

use thiserror::{self, Error};

use std::fs;

#[derive(Error, Debug)]
pub enum PreprocessorError {
    #[error("Invalid file include inside the preprocessor")]
    InvalidInclude,
    #[error("The preprocessor doesn't support including itself")]
    IncludeItself,
}

pub type PreprocessorResult<T> = Result<T, PreprocessorError>;

pub struct Preprocessor{
    directive_table: Vec<String>,
    alias_table: HashMap<String, String>,
    source: String,
    file_name: String,
}

//very basic text processor
impl Preprocessor{
    pub fn new(file_name: String) -> Self{
        Self{
            directive_table: Vec::new(),
            alias_table: HashMap::new(),
            file_name,
            source: String::new(),
        }
    }

    pub fn set_file_name(&mut self, name: String){
        self.file_name = name;
    }

    pub fn with_aliases(file_name: String, alias_table: HashMap<String, String>) -> Self { 
        Self{
            directive_table: Vec::new(),
            alias_table, 
            file_name,
            source: String::new()
        }
    }

    fn capture_directives(&mut self){
        let mut source_without_pre = String::new();

        let mut buf = String::new();

        let mut inside = false;

        for (i, c) in self.source.chars().enumerate(){
            if c == '#'{
                if inside{
                    self.directive_table.push(buf.clone());

                    inside = false;
                } else{
                    if self.source[i+1..].starts_with("include"){
                        source_without_pre += "#include#";
                    }

                    inside = true;
                }

                buf.clear();
            } else{
                if inside{
                    buf.push(c);
                } else{
                    source_without_pre.push(c);
                }
            }
        }

        self.source = source_without_pre;
    }

    fn resolve_directives(&mut self) -> PreprocessorResult<()>{
        for dir in &self.directive_table{
            let pair = dir.split_once(':');

            if dir.trim_start().starts_with("include"){
                if let Some((_, second)) = pair{
                    let file_name = second.trim().to_owned();

                    if file_name == self.file_name{
                        return Err(PreprocessorError::IncludeItself);
                    }

                    let contents = match fs::read_to_string(&file_name){
                        Ok(data) => data,
                        Err(_) => return Err(PreprocessorError::InvalidInclude)
                    };

                    //if a file includes another, we need to handle that
                    let mut np = Preprocessor::with_aliases(file_name, self.alias_table.clone());

                    let result = np.work(contents)?;

                    self.source = self.source.replacen("#include#", &result, 1);
                }
            } else{
                if let Some((first, second)) = pair{
                    self.alias_table.insert(first.trim().to_owned(), second.trim().to_owned());
                }
            }
        }

        Ok(())
    }

    fn replace_aliases(&mut self) {
        for (key, val) in &self.alias_table{
            self.source = self.source.replace(key, val);
        }
    }

    //the preprocessor handles all aliases in a crude manner
    pub fn work(&mut self, source: String) -> PreprocessorResult<String>{
        self.source = source;

        self.capture_directives();

        self.resolve_directives()?;

        self.replace_aliases();

        Ok(self.source.clone())
    }
}

