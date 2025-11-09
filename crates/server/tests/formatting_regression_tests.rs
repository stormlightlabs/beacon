//! Regression Tests for Python Formatter
//!
//! Real-world Python code samples to ensure the formatter handles various edge cases.
//! These tests focus on patterns that the formatter currently handles correctly.
//!
//! Refer to `docs/src/testing.md` for the current formatter testing strategy.

use beacon_lsp::formatting::{Formatter, FormatterConfig};
use beacon_parser::PythonParser;

fn format_code(source: &str) -> String {
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    formatter
        .format_range(source, 0, source.lines().count())
        .expect("Formatting failed")
}

/// Verify formatting is idempotent
fn assert_idempotent(source: &str, description: &str) {
    let first = format_code(source);
    let second = format_code(&first);
    assert_eq!(
        first, second,
        "{description} should be idempotent.\nFirst:\n{first}\nSecond:\n{second}"
    );
}

#[test]
fn test_simple_django_model() {
    let source = r#"from django.db import models

class User(models.Model):
    email=models.EmailField(unique=True)
    bio=models.TextField(blank=True)

    def __str__(self):
        return self.email
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("from django.db import models"));
    assert!(formatted.contains("class User(models.Model):"));
    assert!(formatted.contains("def __str__(self):"));
    assert_idempotent(source, "Simple Django model");
}

#[test]
fn test_fastapi_endpoint() {
    let source = r#"from fastapi import FastAPI,Depends,HTTPException
from pydantic import BaseModel
from typing import List,Optional

app=FastAPI()

class Item(BaseModel):
    name:str
    description:Optional[str]=None
    price:float

async def read_item(item_id:int,q:Optional[str]=None):
    if q:
        return {"item_id":item_id,"q":q}
    return {"item_id":item_id}
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("from fastapi import FastAPI, Depends, HTTPException"));
    assert!(formatted.contains("name: str"));
    assert!(formatted.contains("description: Optional[str] = None"));
    assert!(formatted.contains("async def read_item(item_id: int, q: Optional[str] = None):"));
    assert_idempotent(source, "FastAPI endpoint");
}

#[test]
fn test_data_processing_functions() {
    let source = r#"def process_items(data:list)->list:
    result=[]
    for item in data:
        if item>0:
            result.append(item*2)
    return result

def filter_data(items:list,threshold:int)->list:
    return [x for x in items if x>threshold]
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def process_items(data: list) -> list:"));
    assert!(formatted.contains("result = []"));
    assert!(formatted.contains("if item > 0:"));
    assert!(formatted.contains("def filter_data(items: list, threshold: int) -> list:"));
    assert_idempotent(source, "Data processing functions");
}

#[test]
fn test_async_context_managers() {
    let source = r#"import asyncio

class AsyncDatabase:
    def __init__(self,url:str):
        self.url=url
        self.connection=None

    async def __aenter__(self):
        self.connection=await self.connect()
        return self.connection

    async def __aexit__(self,exc_type,exc_val,exc_tb):
        if self.connection:
            await self.connection.close()

    async def connect(self):
        await asyncio.sleep(0.1)
        return {"url":self.url}
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def __init__(self, url: str):"));
    assert!(formatted.contains("async def __aenter__(self):"));
    assert!(formatted.contains("async def __aexit__(self, exc_type, exc_val, exc_tb):"));
    assert_idempotent(source, "Async context managers");
}

#[test]
fn test_nested_comprehensions() {
    let source = r#"def matrix_operations():
    matrix=[[i*j for j in range(5)] for i in range(5)]

    flattened=[item for row in matrix for item in row]

    filtered={x for x in flattened if x%2==0}

    mapping={i:i**2 for i in range(10) if i%3==0}

    return matrix,flattened,filtered,mapping
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("matrix = [[i * j for j in range(5)] for i in range(5)]"));
    assert!(formatted.contains("flattened = [item for row in matrix for item in row]"));
    assert!(formatted.contains("filtered = {x for x in flattened if x % 2 == 0}"));
    assert!(formatted.contains("mapping = {i: i ** 2 for i in range(10) if i % 3 == 0}"));
    assert_idempotent(source, "Nested comprehensions");
}

#[test]
fn test_simple_dataclass() {
    let source = r#"from dataclasses import dataclass
from typing import List

@dataclass
class Product:
    name:str
    price:float
    tags:List[str]

    def total_cost(self,quantity:int)->float:
        return self.price*quantity
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("from dataclasses import dataclass"));
    assert!(formatted.contains("@dataclass"));
    assert!(formatted.contains("name: str"));
    assert!(formatted.contains("price: float"));
    assert!(formatted.contains("def total_cost(self, quantity: int) -> float:"));
    assert_idempotent(source, "Simple dataclass");
}

#[test]
fn test_exception_handling_patterns() {
    let source = r#"import logging

logger=logging.getLogger(__name__)

def process_data(data:dict)->dict:
    try:
        result=validate_data(data)
        return result
    except ValueError as e:
        logger.error(f"Error: {e}")
        raise
    except KeyError:
        return None
    finally:
        cleanup()

def validate_data(data:dict)->dict:
    if not data:
        raise ValueError("Data cannot be empty")
    return data

def cleanup():
    pass
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("logger = logging.getLogger(__name__)"));
    assert!(formatted.contains("def process_data(data: dict) -> dict:"));
    assert!(formatted.contains("except ValueError as e:"));
    assert_idempotent(source, "Exception handling");
}

#[test]
fn test_simple_class_hierarchy() {
    let source = r#"class Animal:
    def __init__(self,name:str):
        self.name=name

    def speak(self)->str:
        return "Some sound"

class Dog(Animal):
    def __init__(self,name:str,breed:str):
        super().__init__(name)
        self.breed=breed

    def speak(self)->str:
        return "Woof!"
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def __init__(self, name: str):"));
    assert!(formatted.contains("def speak(self) -> str:"));
    assert!(formatted.contains("class Dog(Animal):"));
    assert!(formatted.contains("super().__init__(name)"));
    assert_idempotent(source, "Simple class hierarchy");
}

#[test]
fn test_simple_decorators() {
    let source = r#"def timer(func):
    def wrapper():
        result=func()
        return result
    return wrapper

@timer
def compute():
    return 42
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def timer(func):"));
    assert!(formatted.contains("def wrapper():"));
    assert!(formatted.contains("@timer"));
    assert!(formatted.contains("def compute():"));
    assert_idempotent(source, "Simple decorators");
}

#[test]
fn test_type_annotations_basic() {
    let source = r#"from typing import Union,Optional,List,Dict

def parse_value(value:Union[str,int])->int:
    if isinstance(value,str):
        return int(value)
    return value

def find_item(items:List[int],target:int)->Optional[int]:
    for item in items:
        if item==target:
            return item
    return None
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("from typing import Union, Optional, List, Dict"));
    assert!(formatted.contains("def parse_value(value: Union[str, int]) -> int:"));
    assert!(formatted.contains("def find_item(items: List[int], target: int) -> Optional[int]:"));
    assert_idempotent(source, "Type annotations");
}

#[test]
fn test_context_managers() {
    let source = r#"class FileManager:
    def __init__(self,filename:str):
        self.filename=filename
        self.file=None

    def __enter__(self):
        self.file=open(self.filename,'r')
        return self.file

    def __exit__(self,exc_type,exc_val,exc_tb):
        if self.file:
            self.file.close()

def read_file(filename:str)->str:
    with FileManager(filename) as f:
        return f.read()
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def __init__(self, filename: str):"));
    assert!(formatted.contains("def __enter__(self):"));
    assert!(formatted.contains("def __exit__(self, exc_type, exc_val, exc_tb):"));
    assert!(formatted.contains("with FileManager(filename) as f:"));
    assert_idempotent(source, "Context managers");
}

#[test]
fn test_typevar_assignments() {
    let source = r#"from typing import TypeVar,Generic,Protocol

T=TypeVar('T')
K=TypeVar('K',covariant=True)
ValueType=TypeVar('ValueType',bound=int)

class Repository(Protocol[ValueType]):
    def get(self,key:str)->ValueType:
        ...
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("from typing import TypeVar, Generic, Protocol"));
    assert!(formatted.contains("T = TypeVar(\"T\")"));
    assert!(formatted.contains("K = TypeVar(\"K\", covariant=True)"));
    assert!(formatted.contains("ValueType = TypeVar(\"ValueType\", bound=int)"));
    assert!(formatted.contains("class Repository(Protocol[ValueType]):"));
    assert!(formatted.contains("def get(self, key: str) -> ValueType:"));
    assert_idempotent(source, "TypeVar assignments");
}
#[test]
fn test_string_formatting() {
    let source = r#"def format_message(name:str,age:int)->str:
    basic=f"Hello {name}"
    detailed=f"{name} is {age} years old"
    formatted="Name: {}".format(name)
    return detailed
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def format_message(name: str, age: int) -> str:"));
    assert!(formatted.contains("basic = f\"Hello {name}\""));
    assert!(formatted.contains("detailed = f\"{name} is {age} years old\""));
    assert_idempotent(source, "String formatting");
}

#[test]
fn test_data_science_method_calls() {
    let source = r#"import pandas as pd

def clean(df):
    features=df.drop('target',axis=1)
    normalized=features.fillna(method='ffill',limit=2)
    grouped=normalized.groupby('region',as_index=False)
    summary=grouped.agg(total=('value','sum'))
    return summary
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("features = df.drop(\"target\", axis=1)"));
    assert!(formatted.contains("normalized = features.fillna(method=\"ffill\", limit=2)"));
    assert!(formatted.contains("grouped = normalized.groupby(\"region\", as_index=False)"));
    assert!(formatted.contains("summary = grouped.agg(total=(\"value\", \"sum\"))"));
    assert_idempotent(source, "Data science method calls");
}

#[test]
fn test_complex_lambda_and_functional() {
    let source = r#"from functools import reduce

compose=lambda *funcs: reduce(lambda f,g: lambda x:f(g(x)),funcs,lambda x:x)
pipeline=lambda data: compose(lambda x: x.strip(),lambda x:x.lower())(data)
"#;

    let formatted = format_code(source);

    assert!(formatted.contains("compose = lambda *funcs: reduce(lambda f, g: lambda x: f(g(x)), funcs, lambda x: x)"));
    assert!(formatted.contains("pipeline = lambda data: compose(lambda x: x.strip(),lambda x:x.lower())(data)"));
    assert_idempotent(source, "Complex lambda and functional pipelines");
}

#[test]
fn test_control_flow_structures() {
    let source = r#"def process(value:int)->str:
    if value<0:
        return "negative"
    elif value==0:
        return "zero"
    else:
        return "positive"

def loop_example(items:list):
    for item in items:
        if item%2==0:
            continue
        print(item)

    i=0
    while i<len(items):
        print(items[i])
        i+=1
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("if value < 0:"));
    assert!(formatted.contains("elif value == 0:"));
    assert!(formatted.contains("for item in items:"));
    assert!(formatted.contains("while i < len(items):"));
    assert_idempotent(source, "Control flow structures");
}

#[test]
fn test_imports_and_module_structure() {
    let source = r#"import sys
import os
from typing import List,Dict
from collections import defaultdict

def main():
    data=defaultdict(list)
    return data
"#;

    let formatted = format_code(source);
    let os_pos = formatted.find("import os").unwrap();
    let sys_pos = formatted.find("import sys").unwrap();
    assert!(os_pos < sys_pos, "Imports should be alphabetically sorted");
    assert!(formatted.contains("from typing import List, Dict"));
    assert!(formatted.contains("data = defaultdict(list)"));
    assert_idempotent(source, "Imports and module structure");
}

#[test]
fn test_real_world_api_client() {
    let source = r#"import aiohttp
import asyncio
from typing import Optional,Dict

class APIClient:
    def __init__(self,base_url:str):
        self.base_url=base_url
        self.session=None

    async def __aenter__(self):
        self.session=aiohttp.ClientSession()
        return self

    async def __aexit__(self,exc_type,exc_val,exc_tb):
        if self.session:
            await self.session.close()

    async def get(self,endpoint:str)->Dict:
        url=f"{self.base_url}/{endpoint}"
        async with self.session.get(url) as response:
            return await response.json()
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def __init__(self, base_url: str):"));
    assert!(formatted.contains("async def __aenter__(self):"));
    assert!(formatted.contains("async def get(self, endpoint: str) -> Dict:"));
    assert_idempotent(source, "Real-world API client");
}

#[test]
fn test_dictionary_and_list_operations() {
    let source = r#"def process_data():
    data={"name":"Alice","age":30,"city":"NYC"}
    numbers=[1,2,3,4,5]

    filtered=[x for x in numbers if x>2]
    mapped={k:v for k,v in data.items() if len(str(v))>2}

    nested={"outer":{"inner":{"value":42}}}

    return filtered,mapped,nested
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("data = {\"name\": \"Alice\", \"age\": 30, \"city\": \"NYC\"}"));
    assert!(formatted.contains("numbers = [1, 2, 3, 4, 5]"));
    assert!(formatted.contains("filtered = [x for x in numbers if x > 2]"));
    assert_idempotent(source, "Dictionary and list operations");
}

#[test]
fn test_generators_and_yield() {
    let source = r#"def fibonacci(n:int):
    a,b=0,1
    for _ in range(n):
        yield a
        a,b=b,a+b

def process_chunks(data:list,size:int):
    for i in range(0,len(data),size):
        yield data[i:i+size]
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def fibonacci(n: int):"));
    assert!(formatted.contains("a, b = 0, 1"));
    assert!(formatted.contains("yield a"));
    assert!(formatted.contains("def process_chunks(data: list, size: int):"));
    assert_idempotent(source, "Generators and yield");
}

#[test]
fn test_walrus_operator_patterns() {
    let source = r#"while (line:=f.readline()):
    process(line)

if (n:=len(data))>10:
    print(f"Large dataset: {n}")
"#;

    let formatted = format_code(source);
    let expected = r#"while (line := f.readline()):
    process(line)
if (n := len(data)) > 10:
    print(f"Large dataset: {n}")"#;

    assert_eq!(
        formatted, expected,
        "Walrus formatting should normalize spaces and structure"
    );
    assert_idempotent(source, "Walrus operator");
}

#[test]
fn test_match_statement_python310() {
    let source = r#"match command:
    case {"action":"create","name":name}:
        return create_item(name)
    case {"action":"delete","id":item_id}:
        return delete_item(item_id)
    case _:
        return "Unknown action"
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("match command:"));
    assert!(formatted.contains(r#"case {"action": "create", "name": name}:"#));
    assert!(formatted.contains(r#"case {"action": "delete", "id": item_id}:"#));
    assert_idempotent(source, "Match statement with dict patterns");
}

#[test]
fn test_decorator_patterns() {
    let source = r#"def timer(func):
    def wrapper(*args,**kwargs):
        result=func(*args,**kwargs)
        return result
    return wrapper

@timer
def compute(x:int,y:int,*extras,**options):
    return x+y
"#;

    let formatted = format_code(source);
    assert!(formatted.contains("def wrapper(*args, **kwargs):"));
    assert!(formatted.contains("result = func(*args, **kwargs)"));
    assert!(formatted.contains("def compute(x: int, y: int, *extras, **options):"));
    assert_idempotent(source, "Decorator patterns with args/kwargs");
}
