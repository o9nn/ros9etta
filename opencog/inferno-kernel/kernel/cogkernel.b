implement CogKernel;

include "sys.m";
    sys: Sys;
include "draw.m";
include "cogkernel.m";

# Initialize module on load
init(nil: ref Draw->Context, nil: list of string)
{
    sys = load Sys Sys->PATH;
}

# Initialize cognitive system
CogKernel.init(): ref CogSystem
{
    sys->print("Cognitive Kernel: Initializing...\n");
    
    # Create new cognitive system
    cogsys := ref CogSystem;
    
    # Initialize AtomSpace (hypergraph memory)
    cogsys.atomspace = ref AtomSpace(nil, nil, ref Index(nil, nil, nil));
    sys->print("  AtomSpace initialized\n");
    
    # Initialize PLN (reasoning engine)
    cogsys.pln = ref PLN;
    sys->print("  PLN initialized\n");
    
    # Initialize ECAN (attention allocation)
    bank := ref AttentionBank(1000.0, nil);
    cogsys.ecan = ref ECAN(bank);
    sys->print("  ECAN initialized\n");
    
    # Initialize CogLoop (cognitive cycle)
    cogsys.cogloop = ref CogLoop("ready", 0);
    sys->print("  CogLoop initialized\n");
    
    # Set default configuration
    cogsys.config = ref Config(10000, 1000.0, 0.01, 0.1);
    
    sys->print("Cognitive Kernel: Ready\n");
    return cogsys;
}

# Shutdown cognitive system
CogKernel.shutdown(sys: ref CogSystem): int
{
    if (sys == nil)
        return -1;
    
    sys = nil;  # Release memory
    return 0;
}

# TruthValue operations

TruthValue.new(s: real, c: real): ref TruthValue
{
    if (s < 0.0) s = 0.0;
    if (s > 1.0) s = 1.0;
    if (c < 0.0) c = 0.0;
    if (c > 1.0) c = 1.0;
    
    return ref TruthValue(s, c);
}

TruthValue.revise(tv1: self ref TruthValue, tv2: ref TruthValue): ref TruthValue
{
    # Revision rule: combine two truth values
    # Weighted average based on confidence
    
    if (tv1 == nil || tv2 == nil)
        return tv1;
    
    w1 := tv1.confidence;
    w2 := tv2.confidence;
    total := w1 + w2;
    
    if (total == 0.0)
        return tv1;
    
    new_strength := (tv1.strength * w1 + tv2.strength * w2) / total;
    new_confidence := total / (1.0 + total);  # Confidence increases
    
    return ref TruthValue(new_strength, new_confidence);
}

TruthValue.conjunction(tv1: self ref TruthValue, tv2: ref TruthValue): ref TruthValue
{
    # Conjunction: AND of two truth values
    
    if (tv1 == nil || tv2 == nil)
        return ref TruthValue(0.0, 0.0);
    
    # Strength: product (independent probabilities)
    new_strength := tv1.strength * tv2.strength;
    
    # Confidence: minimum
    new_confidence := tv1.confidence;
    if (tv2.confidence < new_confidence)
        new_confidence = tv2.confidence;
    
    return ref TruthValue(new_strength, new_confidence);
}

# AtomSpace operations

AtomSpace.create(a: self ref AtomSpace, name: string, tv: ref TruthValue): ref Atom
{
    if (a == nil || name == nil || tv == nil)
        return nil;
    
    # Generate unique ID
    id := len a.atoms;
    
    # Create new atom
    atom := ref Atom(id, name, "Concept", tv, 0.0, 0.0, 
                     ref AttentionValue(0.0, 0.0, 0));
    
    # Add to atomspace
    a.atoms = atom :: a.atoms;
    
    # Update index
    if (a.index != nil) {
        a.index.byname = (name, atom) :: a.index.byname;
    }
    
    return atom;
}

AtomSpace.link(a: self ref AtomSpace, source: ref Atom, target: ref Atom,
               linktype: string): ref Link
{
    if (a == nil || source == nil || target == nil)
        return nil;
    
    # Generate unique ID
    id := len a.links;
    
    # Create link with combined truth value
    combined_tv := source.tv.conjunction(target.tv);
    
    link := ref Link(id, linktype, source, target, combined_tv);
    
    # Add to atomspace
    a.links = link :: a.links;
    
    return link;
}

AtomSpace.query(a: self ref AtomSpace, pattern: string): list of ref Atom
{
    if (a == nil)
        return nil;
    
    # Simple pattern matching: find atoms containing pattern
    results: list of ref Atom;
    
    for (atoms := a.atoms; atoms != nil; atoms = tl atoms) {
        atom := hd atoms;
        if (atom != nil && atom.name != nil) {
            # Check if pattern matches atom name
            if (contains(atom.name, pattern))
                results = atom :: results;
        }
    }
    
    return results;
}

AtomSpace.gettruth(a: self ref AtomSpace, atom: ref Atom): ref TruthValue
{
    if (atom != nil)
        return atom.tv;
    return ref TruthValue(0.0, 0.0);
}

# PLN operations

PLN.deduce(pln: self ref PLN, premises: list of ref Atom): list of ref Inference
{
    if (pln == nil || premises == nil)
        return nil;
    
    # Deduction: A->B, B->C => A->C
    # Simplified: combine truth values of premises
    
    results: list of ref Inference;
    
    if (len premises >= 2) {
        p1 := hd premises;
        p2 := hd tl premises;
        
        # Combine truth values
        combined_tv := p1.tv.conjunction(p2.tv);
        
        # Create conclusion atom
        conclusion_name := p1.name + "->" + p2.name;
        conclusion := ref Atom(0, conclusion_name, "ImplicationLink", 
                              combined_tv, 0.0, 0.0, 
                              ref AttentionValue(0.0, 0.0, 0));
        
        # Create inference
        inf := ref Inference(conclusion, premises, "Deduction", combined_tv);
        results = inf :: results;
    }
    
    return results;
}

PLN.induce(pln: self ref PLN, examples: list of ref Atom): list of ref Inference
{
    if (pln == nil || examples == nil)
        return nil;
    
    # Induction: multiple examples => general rule
    # Simplified: create generalization with reduced confidence
    
    results: list of ref Inference;
    
    if (examples != nil) {
        first := hd examples;
        
        # Reduce confidence for induction
        induced_tv := ref TruthValue(first.tv.strength, 
                                     first.tv.confidence * 0.7);
        
        # Create generalized atom
        general := ref Atom(0, "General_" + first.name, "Concept",
                           induced_tv, 0.0, 0.0,
                           ref AttentionValue(0.0, 0.0, 0));
        
        inf := ref Inference(general, examples, "Induction", induced_tv);
        results = inf :: results;
    }
    
    return results;
}

PLN.abduce(pln: self ref PLN, observation: ref Atom, 
           hypothesis: ref Atom): ref Inference
{
    if (pln == nil || observation == nil || hypothesis == nil)
        return nil;
    
    # Abduction: observation + hypothesis => explanation
    # Simplified: combine with reduced confidence
    
    combined_tv := observation.tv.conjunction(hypothesis.tv);
    abduced_tv := ref TruthValue(combined_tv.strength, 
                                 combined_tv.confidence * 0.5);
    
    # Create explanation
    explanation := ref Atom(0, "Explains_" + observation.name, "Concept",
                           abduced_tv, 0.0, 0.0,
                           ref AttentionValue(0.0, 0.0, 0));
    
    premises := observation :: hypothesis :: nil;
    return ref Inference(explanation, premises, "Abduction", abduced_tv);
}

PLN.unify(pln: self ref PLN, pattern: ref Atom, target: ref Atom): ref Substitution
{
    if (pln == nil || pattern == nil || target == nil)
        return nil;
    
    # Simple unification: match pattern with target
    bindings := (pattern.name, target) :: nil;
    tv := ref TruthValue(1.0, 1.0);
    
    return ref Substitution(bindings, tv);
}

# ECAN operations

ECAN.spread(ecan: self ref ECAN, source: ref Atom, amount: real): int
{
    if (ecan == nil || source == nil || ecan.bank == nil)
        return -1;
    
    # Spread activation from source
    if (source.av != nil) {
        source.av.sti = source.av.sti + amount;
        source.sti = source.av.sti;
    }
    
    return 0;
}

ECAN.focus(ecan: self ref ECAN, target: ref Atom): int
{
    if (ecan == nil || target == nil)
        return -1;
    
    # Focus attention on target
    return ecan.spread(target, 100.0);
}

ECAN.stimulate(ecan: self ref ECAN, atoms: list of ref Atom): int
{
    if (ecan == nil || atoms == nil)
        return -1;
    
    # Stimulate multiple atoms
    count := 0;
    for (a := atoms; a != nil; a = tl a) {
        atom := hd a;
        if (atom != nil) {
            ecan.spread(atom, 10.0);
            count++;
        }
    }
    
    return count;
}

ECAN.importance(ecan: self ref ECAN, atom: ref Atom): real
{
    if (atom != nil && atom.av != nil)
        return atom.av.sti;
    return 0.0;
}

# AttentionBank operations

AttentionBank.allocate(bank: self ref AttentionBank, atom: ref Atom, sti: real): int
{
    if (bank == nil || atom == nil)
        return -1;
    
    # Allocate STI to atom
    if (atom.av == nil)
        atom.av = ref AttentionValue(0.0, 0.0, 0);
    
    atom.av.sti = sti;
    atom.sti = sti;
    
    return 0;
}

AttentionBank.decay(bank: self ref AttentionBank, rate: real): int
{
    if (bank == nil)
        return -1;
    
    # Decay attention values
    for (atoms := bank.atoms; atoms != nil; atoms = tl atoms) {
        atom := hd atoms;
        if (atom != nil && atom.av != nil) {
            atom.av.sti = atom.av.sti * (1.0 - rate);
            atom.sti = atom.av.sti;
        }
    }
    
    return 0;
}

AttentionBank.normalize(bank: self ref AttentionBank): int
{
    if (bank == nil)
        return -1;
    
    # Normalize STI values to total budget
    sum := 0.0;
    for (atoms := bank.atoms; atoms != nil; atoms = tl atoms) {
        atom := hd atoms;
        if (atom != nil && atom.av != nil)
            sum = sum + atom.av.sti;
    }
    
    if (sum > 0.0) {
        scale := bank.total_sti / sum;
        for (atoms := bank.atoms; atoms != nil; atoms = tl atoms) {
            atom := hd atoms;
            if (atom != nil && atom.av != nil) {
                atom.av.sti = atom.av.sti * scale;
                atom.sti = atom.av.sti;
            }
        }
    }
    
    return 0;
}

# CogLoop operations

CogLoop.perceive(loop: self ref CogLoop, stimuli: list of ref Atom): int
{
    if (loop == nil)
        return -1;
    
    loop.state = "perceiving";
    return len stimuli;
}

CogLoop.reason(loop: self ref CogLoop): list of ref Inference
{
    if (loop == nil)
        return nil;
    
    loop.state = "reasoning";
    return nil;  # Would call PLN here
}

CogLoop.act(loop: self ref CogLoop, actions: list of ref Atom): int
{
    if (loop == nil)
        return -1;
    
    loop.state = "acting";
    return len actions;
}

CogLoop.reflect(loop: self ref CogLoop): ref Insight
{
    if (loop == nil)
        return nil;
    
    loop.state = "reflecting";
    loop.cycle++;
    
    content := "Completed cycle " + string loop.cycle;
    return ref Insight(content, nil, ref TruthValue(1.0, 1.0));
}

# Cognitive syscalls

CogKernel.sys_think(sys: ref CogSystem, context: string): string
{
    if (sys == nil)
        return "Error: no cognitive system";
    
    # Thinking: perceive context, reason about it, generate thought
    # Query atomspace for relevant atoms
    relevant := sys.atomspace.query(context);
    
    if (relevant == nil || len relevant == 0)
        return "No knowledge about: " + context;
    
    # Find most important atom
    # Use very low initial value to ensure any atom is selected
    MIN_STI := -1000.0;
    best: ref Atom = nil;
    max_sti := MIN_STI;
    
    for (atoms := relevant; atoms != nil; atoms = tl atoms) {
        atom := hd atoms;
        if (atom != nil && atom.sti > max_sti) {
            best = atom;
            max_sti = atom.sti;
        }
    }
    
    if (best != nil) {
        # Perform inference on the best atom
        premises := best :: nil;
        inferences := sys.pln.deduce(premises);
        
        if (inferences != nil) {
            inf := hd inferences;
            return "Thought: " + context + " relates to " + best.name + 
                   " (concluded: " + inf.conclusion.name + ")";
        }
        
        return "Thought: " + context + " strongly relates to " + best.name;
    }
    
    return "Thought about: " + context;
}

CogKernel.sys_reason(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference
{
    if (sys == nil || sys.pln == nil)
        return nil;
    
    return sys.pln.deduce(premises);
}

CogKernel.sys_learn(sys: ref CogSystem, patterns: list of string): int
{
    if (sys == nil || sys.atomspace == nil)
        return -1;
    
    # Learning: create atoms for each pattern
    count := 0;
    for (p := patterns; p != nil; p = tl p) {
        pattern := hd p;
        tv := ref TruthValue(0.8, 0.6);
        sys.atomspace.create(pattern, tv);
        count++;
    }
    
    return count;
}

CogKernel.sys_attend(sys: ref CogSystem, focus: ref Atom): int
{
    if (sys == nil || sys.ecan == nil)
        return -1;
    
    return sys.ecan.focus(focus);
}

CogKernel.sys_reflect(sys: ref CogSystem): ref Insight
{
    if (sys == nil || sys.cogloop == nil)
        return nil;
    
    return sys.cogloop.reflect();
}

# AtomSpace syscalls

CogKernel.sys_atom_create(sys: ref CogSystem, name: string, 
                          tv: ref TruthValue): ref Atom
{
    if (sys == nil || sys.atomspace == nil)
        return nil;
    
    return sys.atomspace.create(name, tv);
}

CogKernel.sys_atom_link(sys: ref CogSystem, source: ref Atom,
                        target: ref Atom, linktype: string): ref Link
{
    if (sys == nil || sys.atomspace == nil)
        return nil;
    
    return sys.atomspace.link(source, target, linktype);
}

CogKernel.sys_atom_query(sys: ref CogSystem, pattern: string): list of ref Atom
{
    if (sys == nil || sys.atomspace == nil)
        return nil;
    
    return sys.atomspace.query(pattern);
}

# PLN syscalls

CogKernel.sys_deduce(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference
{
    if (sys == nil || sys.pln == nil)
        return nil;
    
    return sys.pln.deduce(premises);
}

CogKernel.sys_induce(sys: ref CogSystem, examples: list of ref Atom): list of ref Inference
{
    if (sys == nil || sys.pln == nil)
        return nil;
    
    return sys.pln.induce(examples);
}

CogKernel.sys_abduce(sys: ref CogSystem, observation: ref Atom,
                     hypothesis: ref Atom): ref Inference
{
    if (sys == nil || sys.pln == nil)
        return nil;
    
    return sys.pln.abduce(observation, hypothesis);
}

# ECAN syscalls

CogKernel.sys_spread_activation(sys: ref CogSystem, source: ref Atom, amount: real): int
{
    if (sys == nil || sys.ecan == nil)
        return -1;
    
    return sys.ecan.spread(source, amount);
}

CogKernel.sys_focus(sys: ref CogSystem, target: ref Atom): int
{
    if (sys == nil || sys.ecan == nil)
        return -1;
    
    return sys.ecan.focus(target);
}

CogKernel.sys_importance(sys: ref CogSystem, atom: ref Atom): real
{
    if (sys == nil || sys.ecan == nil)
        return 0.0;
    
    return sys.ecan.importance(atom);
}

# Utility functions

CogKernel.newtruthvalue(strength: real, confidence: real): ref TruthValue
{
    return TruthValue.new(strength, confidence);
}

CogKernel.createatom(sys: ref CogSystem, name: string, tv: ref TruthValue): ref Atom
{
    return sys_atom_create(sys, name, tv);
}

CogKernel.deduce(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference
{
    return sys_deduce(sys, premises);
}

CogKernel.learn(sys: ref CogSystem, patterns: list of string): int
{
    return sys_learn(sys, patterns);
}

CogKernel.think(sys: ref CogSystem, context: string): string
{
    return sys_think(sys, context);
}

# Helper function to check if string contains pattern
contains(s: string, pattern: string): int
{
    if (s == nil || pattern == nil)
        return 0;
    
    # Simple substring search
    slen := len s;
    plen := len pattern;
    
    if (plen > slen)
        return 0;
    
    for (i := 0; i <= slen - plen; i++) {
        match := 1;
        for (j := 0; j < plen; j++) {
            if (s[i+j] != pattern[j]) {
                match = 0;
                break;
            }
        }
        if (match)
            return 1;
    }
    
    return 0;
}
