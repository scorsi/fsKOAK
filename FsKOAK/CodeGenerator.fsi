namespace Koak

module CodeGenerator = 
    type Result<'a> = 
        | Success of 'a
        | Failure of string
    
    val codegen : Parser.Node list -> Result<'a>
