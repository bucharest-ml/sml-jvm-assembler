# Work Log

## 2023-04-24 09:56:20

> Each stack map frame described in the entries table relies on the previous
> frame for some of its semantics. The first stack map frame of a method is
> implicit, and computed from the method descriptor by the type checker
> (§4.10.1.6). The stack_map_frame structure at entries[0] therefore describes
> the second stack map frame of the method.

— JVMS20, §4.7.4

Okay... so I'll have to generate all the stack frames and just after that drop
the ones that are not required by the specification:

> It is illegal to have code after an **unconditional branch** without a stack
> map frame being provided for it.

— JVMS20, §4.10.1.6

> • Conditional branch: ifeq, ifne, iflt, ifle, ifgt, ifge, ifnull, ifnonnull, if_icmpeq, if_icmpne, if_icmplt, if_icmple, if_icmpgt if_icmpge, if_acmpeq, if_acmpne.
> • Compound conditional branch: tableswitch, lookupswitch.
> • **Unconditional branch**: goto, goto_w, jsr, jsr_w, ret.

— JVMS20, §2.11.7

Steps:

  1. Build a control-flow graph of the instructions
  2. Basic blocks

Could we generate the StackMap frames without building the CFG? And only use
a CFG when we want to keep just the required frames?

---

From: From Stack Maps to Software Certificates [slides], slide 6

> [Java bytecode verification] is formalized as a data flow problem.

---

Slide 27:

> BCV typing with interfaces
> Two kind of reference types : classes and interfaces.
> Interfaces introduces a form of intersection types. that must be represented in the type hierarchy.
> The byte code verifier opts for an alternative solution :
> Treat interfaces as java.lang.Object and defer type checking to run-time.
> Type checking rule :
> isJavaAssignable(class(_,_), class(To, L)) :- loadedClass(To, L, ToClass), classIsInterface(ToClass).

## Efficient Bytecode Verification and Compilation in a Virtual Machine

Figure 2.1, page 5

```
todo ← true
while todo = true do
  todo ← false
  for all i in all instructions of a method do
    if i was changed then
      todo ← true
      check whether stack and local variable types match definition of i
      calculate new state after i
      for all s in all successor instructions of i do
        if current state for s ̸= new state derived from i then
          assume state after i as new entry state for s
          mark s as changed
        end if
      end for
    end if
  end for
end while
```

## 2023-04-22 22:06:59

Taken from a draft in igstan.ro:

---
title: JVM Bytecode Verification
author: Ionuț G. Stan
---

I'll briefly describe the process of bytecode verification that the JVM
performs during the loading of .class files.

## Motivation

Why was verification needed?

## Complications

There's a hierarchy of verification types that induces a notion of subtyping.
Store and load instructions need to check subtyping conformance and this
requires loading external classes into the system. I'm hoping to avoid this
somehow, as it requires the side-effect of reading files form disk. In
addition, it means that I might need to write a parser for .class files and a
JAR (which are ZIP archives) reader.

## 2022-04-16 11:37:19

> An additional problem with compile-time checking is **version skew**. A user
> may have successfully compiled a class, say PurchaseStockOptions, to be a
> subclass of TradingClass. But the definition of TradingClass might have
> changed since the time the class was compiled in a way that is not compatible
> with pre-existing binaries.

— JVMS8, §4.10

> The intent is that a **stack map frame** must appear at the beginning of each
> **basic block** in a method. The stack map frame specifies the verification
> type of each operand stack entry and of each local variable at the start of
> each basic block.

— JVMS8, §4.10.1

### Read

  - Lightweight Bytecode Verification

## 2022-04-17 14:09:19

### Java Bytecode Verification — An Overview

For every instruction `i`:

```
i : in(i) → out(i)
in(i) = lub { out(j) | j predecessor i }
```

i₀ = first instruction

```
in(i₀) = (ε, (P₀ ... Pₙ₋₁, ⊤ ... ⊤))
```

Pₖ are the types of the parameter methods

> The dataflow framework presented above requires that the type algebra,
> ordered by the subtyping relation, constitutes a semi-lattice.
— §3.3

### Dataflow Analysis [slides]

> Available expressions is a **forward must** analysis
>
> • **Forward** = Data flow from in to out
> • **Must** = At join points, only keep facts that hold on all paths that are joined

> Liveness is a **backwards may** analysis
> • To know if a variable is live, we need to look at the future uses of it. We
>   propagate facts backwards, from Out to In.
> • Variable is live if it is used on some path
