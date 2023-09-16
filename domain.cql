# see options button to disable spellcheck or outline 

#global options at start of file, see manual or options button
options
	timeout = 30

###### Kind comment

html { (* "some html" *) } #be sure to escape slashes and quotes
md { (* "some markdown" *) }

###### Kind graph

graph g1 = literal {
	nodes
		n1 n2
	edges
		f : n1 -> n2
}

###### Kind typeside

typeside ty1 = empty
typeside ty4 = literal {
	imports #order of sections (imports, types, ...) matters, but sections can be omitted
		ty1
	types
		Nat
	constants
		zero : Nat
	functions
		succ : Nat -> Nat
		plus : Nat, Nat -> Nat
	external_types
	 	String -> "java.lang.String"
	external_parsers
	 	String -> "x => x"
	external_functions
	 	append : String, String -> String = "(x, y) => x.concat(y)"
	equations
		forall x. plus(zero, x) = x
	 	forall x, y. plus(succ(x),y) = succ(plus(x,y))
	 options
	 	prover = completion
}

###### Kind schema

schema empty_schema = empty : ty1

schema person_schema = literal : ty4 {
	imports 
		empty_schema
	entities Person
	attributes
		vatnumber: Person -> String  
		name: Person -> String
		gender: Person -> String
	options
	 	prover = completion
}

instance PersonInstance = literal : person_schema {
	generators
		rudolf berislav : Person
	equations
		vatnumber(rudolf) = HR123               vatnumber(berislav) = HR124
		name(rudolf) = rudolf		            name(berislav) = berislav
		gender(rudolf) = male				    gender(berislav) = male                                        
	options
		program_allow_nontermination_unsafe=true
		prover_simplify_max = 512
}

schema company_schema = literal : ty4 {
	imports empty_schema
	entities Company
	attributes
		name: Company -> String
		vat: Company -> String
	options
	 	prover = completion
}

schema employee_schema = literal : ty4 {
	imports empty_schema
	entities Employee
	attributes
		name: Employee -> String
	options
	 	prover = completion
}

schema world_schema = literal : ty4 {
	imports empty_schema
	entities
		Country
		Continent
		City
		Region
	foreign_keys
		is: Country -> Region
		is: Continent -> Region
		is: City -> Region
	attributes
		name: Region -> String
	options
	 	prover = completion
		
}

schema s3 = literal : ty4 {
	imports
		person_schema
		world_schema
		company_schema
		employee_schema
	entities
		Department
	foreign_keys
		manager   : Employee -> Employee
		worksIn   : Employee -> Department
		secretary : Department -> Employee
		is : Person -> Employee
	path_equations
		Employee.manager.manager = Employee.manager
		Employee.manager.worksIn = Employee.worksIn
  		Department.secretary.worksIn = Department
  	attributes
  		employee_name	: Employee -> String
     	age			: Employee -> Nat
     	cummulative_age: Employee -> Nat
     	name 		: Department -> String
     observation_equations
     	forall e. cummulative_age(e) = plus(age(e), age(manager(e)))
	options
		prover = completion
}
