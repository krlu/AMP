# AMP (Automated Meta Programming)
Tools for improving the workflow of Java Developers. Features include:   
- Automatic Refactoring
- Verification/Validation  
- Compile time optimization 

## Requirements 
- scala 2.12.3 (or higher)
- sbt 0.13.8 (or higher)
- Java 11 

## Building the project

### Running unit test  
``` 
> sbt clean compile
> sbt test 
```

### Packaging for command line usage
First run the following  
``` 
> sbt pack
```
This will create an executable batch script that runs an accompanying jar.\
Batch script is located in `amp/target/pack/bin/app.bat`
To see command line usage, simply run `app --help`

You can run the batch script with the following arguments: 

```
> app --print [inputPath] [outputPath]
```
Takes the class specified by the input path and print the AST to the output path (can be any file type).
```
> app --analyze [inputPath]
```
Takes the class specified by the input path and estimates the output of a given method.\
Estimations are printed to Stdout. 
 
```
> --refactor [inputPath] [outputPath]
```
Takes the class specified by the input path, automatically refactors the AST.\
Prints the refactored AST to the output file (must have a java extension)

***Note: all input files must have a java extension***  


