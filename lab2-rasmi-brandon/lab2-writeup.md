# Lab 2 Write Up Portion

Brandon Aguirre
Rasmi Lamichhane

---

### 1. Grammar: Synthetic Examples.

a. This language consists of the non terminals S, A, and B. S is generated by the the non terminals ABA. A is generated by an a or an aA. B is generated by an epsilon (empty string) or a bBc or a BB. The A non terminal can represent strings of repeated a. The B non terminal can represent an empty spaces, or a string of b’s and c’s 

b. For the grammar given: 

1. baab is possible using the grammar.
```
	S =>AaBb
    	=>baBb
    	=>baab
```

2. Sentence bbbab is not possible because there is a capital B in the second to last position. This cannot be converted into a lower case b.

3. Not possible

4. bbaa can be generate through grammer b 
```
	S => AaBb
  		=>AbaBb 
  		=>bbaBb
  		=>bbaab
```

c.  In the grammar given:  

1. 
```
	S => aScB
		=> abcB
		=> abcd

				    S
				 / |  | \
				a  S  c  B
				   |     |
			       b     d

```
2. Not possible because there is a capital B in the last position. This cannot be converted into a lower case b.

3. Not possible because there is a capital B in last position. This cannot be converted into a lower case b.

4. Not possible because there is only three lowercase letter.
 
5. Not possible because there is a capital B in last position. This cannot be converted into c or cancel it out.

d.
```
	    A=>A⊕A
	    	=>A⊕(A⊕A)
	    	or
	    	=>(A⊕A)⊕A 



	    	A 						A
	    /	|	\               /   |   \
	   A 	⊕    A             A    ⊕    A
	 / | \                             / | \
	A  ⊕  A                           A  ⊕  A

```

The grammar is ambiguous because it can be evaluated to the same result in two different ways. Either `A⊕(A⊕A) or (A⊕A)⊕A`. Then either path could evaluate to (for example) aaa.

e.  We can describe the number of a's A has by defining the following set of judgment forms.

```
an axiom ->		-----
				a = 1

an axiom ->		-----
				b = 0


A = a
-----
A = 1


A = b
-----
A = 0


A = A ⊕ A
---------
  A = n


An Example

A = A ⊕ A
	=> A ⊕ A ⊕ A
	=> a ⊕ a ⊕ b
	=> 1 ⊕ 1 ⊕ 0

	n = 2 


``` 



### 2. Grammar Understanding and Language

a. 

i. The first expression uses left recursion to generate a string of repeated operand operator operand operator operand... and so forth.

The second expression creates the same pattern of alternating between operand and operator and terminating with an epsilon. expression uses right recursion.

ii. Yes. These grammars generate the same expressions beside the fact that there is an epsilon terminator on the second expression which by assumption evaluates to an empty string. The only difference being the first expression using left recursion, and the right using a second grammar esuffix to create the expression using right recursion.

b. 

```
scala>  (1 - 1 << 1)
res5: Int = 0

scala> (1 << 1 - 1)
res6: Int = 1
```

Here we see that there is no precedence between '-' and '<<'. There is only right and left associativity. We can tell because the first expression evaluates to 0 which means that the subtraction is coming first (1-1 => 0 << 1 => 0), and the second expression evaluates to 1 which means 1 << 1 is evaluating first.

To show an example of an operation that has precedence over subtraction, I used multiplication in a similar expression.
```
scala> (1 - 1 * 2)
res7: Int = -1

scala> (1 * 2 - 1)
res8: Int = 1

```
Here we see that multiplication comes first in either case.

c. Grammer
```

F::= L.D
L::= -N|N
D::= N|Z|ZD|ND|E-NRε|ENRε
R::= N|Z|ZD|ND
N::= 1|2|3|4|5|6|7|8|9
Z::= 0

```







