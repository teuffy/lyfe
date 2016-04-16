# Building Microservices - Notes

## Chapter 1. Microservices

- Microservices should be small enough but not one line code smaller
- exposed API should be technology-agnostic (ex. REST?)
- Golden rule: _can you make a change to a service and **deploy it by itself without changing anything else?**_ <- answer should be of course yes ;)
- With microservices it is easier to try new programming language, paradigm, database, framework, etc. because one has multiple new places in which to try out a new piece of technology.
- With using shared libraries you loose true technology heterogenity (usually library is one given language, especially internal one). Ease of scaling is undermined. Finally, unless you're using dynamically linked libraries, you cannot deploy a new library without deploying everything else.

## Chapter 2. The evolutionary Architect

- 
