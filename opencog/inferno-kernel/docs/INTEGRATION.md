# Inferno Kernel Integration Guide

## Overview

This guide explains how to integrate the Inferno Cognitive Kernel with existing OpenCog systems and the broader RosettaCog ecosystem.

## Architecture Integration

### Three-Layer Architecture

```
┌─────────────────────────────────────────────────────┐
│  Application Layer (Python/Limbo/Multi-language)    │
│  - OpenCog workbench                                │
│  - Multi-agent reasoning                            │
│  - Hypergraph analysis                              │
│  - FrankenCog synthesis                             │
├─────────────────────────────────────────────────────┤
│  Bridge Layer (Python ↔ Inferno)                    │
│  - inferno_bridge.py                                │
│  - JSON-based communication                         │
│  - Atom/Link/TruthValue translation                 │
├─────────────────────────────────────────────────────┤
│  Kernel Layer (Pure Inferno/Limbo)                  │
│  - Cognitive kernel (cogkernel.m/b)                 │
│  - AtomSpace, PLN, ECAN, CogLoop                    │
│  - Dis bytecode execution                           │
│  - 9P distributed cognition                         │
└─────────────────────────────────────────────────────┘
```

## Integration Methods

### 1. Python Bridge Integration

Use `inferno_bridge.py` to access kernel from Python:

```python
from opencog.inferno_kernel.lib.inferno_bridge import (
    InfernoKernelBridge, TruthValue, Atom
)

# Initialize kernel bridge
kernel = InfernoKernelBridge()
kernel.init()

# Create knowledge
tv = TruthValue(0.9, 0.8)
atom = kernel.create_atom("consciousness", tv)

# Reasoning
inferences = kernel.deduce([atom1, atom2])

# Attention
kernel.focus(atom, 100.0)

# Thinking
thought = kernel.think("what am I?")
```

### 2. Direct Limbo Integration

Write applications directly in Limbo:

```limbo
implement MyApp;

include "sys.m";
    sys: Sys;
include "cogkernel.m";
    cogkernel: CogKernel;

MyApp: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
    sys = load Sys Sys->PATH;
    cogkernel = load CogKernel CogKernel->PATH;
    
    # Use cognitive kernel directly
    cogsys := cogkernel->init();
    tv := cogkernel->newtruthvalue(0.9, 0.8);
    atom := cogkernel->createatom(cogsys, "knowledge", tv);
}
```

### 3. Distributed Integration via 9P

Mount remote cognitive kernels:

```bash
# Mount remote kernel
mount -A tcp!remotehost!9P /mnt/remotemind

# Access remote knowledge
cat /mnt/remotemind/atomspace/nodes/concept_X

# Trigger remote reasoning
echo "premise1 premise2" > /mnt/remotemind/reasoning/deduce

# Read results
cat /mnt/remotemind/reasoning/deductions
```

## OpenCog Component Integration

### Hypergraph Analyzer Integration

```python
from opencog.lib.hypergraph_analyzer import HypergraphAnalyzer
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge

# Initialize both systems
hypergraph = HypergraphAnalyzer('.')
kernel = InfernoKernelBridge()
kernel.init()

# Use hypergraph analysis to guide kernel operations
languages = hypergraph.get_all_languages()
for lang in languages[:10]:
    tv = TruthValue(0.8, 0.7)
    kernel.create_atom(f"language_{lang}", tv)

# Export kernel state
state = kernel.export_state()
print(f"Kernel has {state['statistics']['total_atoms']} atoms")
```

### Multi-Agent Reasoning Integration

```python
from opencog.agents.agent_zero import AgentZero
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge

# Initialize kernel and agent system
kernel = InfernoKernelBridge()
kernel.init()

agent = AgentZero()

# Use Agent-Zero with cognitive kernel
task = {
    'id': 'reasoning-001',
    'description': 'Reason about consciousness',
    'type': 'deductive'
}

# Create knowledge in kernel
tv = TruthValue(0.9, 0.8)
consciousness = kernel.create_atom("consciousness", tv)
awareness = kernel.create_atom("awareness", tv)

# Perform reasoning
inferences = kernel.deduce([consciousness, awareness])

# Agent-Zero can leverage kernel inferences
result = agent.analyze_task(task)
```

### Atom Type System Integration

```python
from opencog.lib.atom_type_builder import AtomTypeBuilder
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge

# Initialize systems
builder = AtomTypeBuilder('.')
kernel = InfernoKernelBridge()
kernel.init()

# Build atom type system
system = builder.build()

# Map cognitive domains to kernel atoms
for domain in system.cognitive_domains:
    tv = TruthValue(0.9, 0.9)
    kernel.create_atom(f"domain_{domain.name}", tv)

# Export state
state = kernel.export_state()
```

## RosettaCog Ecosystem Integration

### FrankenCog Manifest Integration

The cognitive kernel can implement optimal language selections:

```python
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge

kernel = InfernoKernelBridge()
kernel.init()

# FrankenCog selections as kernel knowledge
manifest = {
    'symbolic_reasoning': 'C#',
    'pattern_recognition': 'Ada',
    'knowledge_representation': 'C++',
    'machine_learning': 'C',
    'natural_language': 'C',
    'planning': '11l',
    'uncertainty_reasoning': 'C',
    'cognitive_architecture': 'Ada',
    'perception_motor': 'C',
    'meta_learning': 'FreeBASIC'
}

# Store in kernel
for domain, language in manifest.items():
    tv = TruthValue(1.0, 0.95)  # High certainty
    domain_atom = kernel.create_atom(f"domain_{domain}", tv)
    lang_atom = kernel.create_atom(f"language_{language}", tv)
    kernel.create_link(domain_atom, lang_atom, "optimal_for")

print(f"FrankenCog manifest loaded: {len(manifest)} mappings")
```

### Language Evaluation Integration

```python
from opencog.lib.opencog_analyzer import OpenCogAnalyzer
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge

analyzer = OpenCogAnalyzer('.')
kernel = InfernoKernelBridge()
kernel.init()

# Analyze languages and store in kernel
languages = analyzer.get_all_languages()
for lang in languages[:100]:  # Process subset
    tasks = analyzer.get_language_tasks(lang)
    task_count = len(tasks)
    
    # Truth value based on task coverage
    strength = min(1.0, task_count / 1000.0)
    confidence = 0.8
    tv = TruthValue(strength, confidence)
    
    kernel.create_atom(f"language_{lang}", tv)

print(f"Analyzed {len(languages)} languages")
```

## Command-Line Integration

### New CLI Tools

Add kernel commands to OpenCog CLI:

```bash
# Boot cognitive kernel
opencog/inferno-kernel/boot/cogboot

# Run examples
opencog/inferno-kernel/examples/self_aware
opencog/inferno-kernel/examples/distributed_mind

# Python bridge demo
python3 opencog/inferno-kernel/lib/inferno_bridge.py
```

### Integration with Existing Tools

```bash
# Use with opencog-analyze
opencog/bin/opencog-analyze | python3 -c "
from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge
import sys
kernel = InfernoKernelBridge()
kernel.init()
# Process analysis results...
"

# Use with opencog-hypergraph
opencog/bin/opencog-hypergraph --all
python3 opencog/inferno-kernel/lib/inferno_bridge.py

# Use with opencog-agent-zero
opencog/bin/opencog-agent-zero task-001 autonomous
# Agent-Zero can leverage cognitive kernel
```

## Data Format Integration

### JSON Export/Import

Kernel state is compatible with OpenCog JSON formats:

```python
# Export from kernel
state = kernel.export_state()
with open('cognitive_state.json', 'w') as f:
    json.dump(state, f, indent=2)

# Import to kernel
with open('cognitive_state.json', 'r') as f:
    state = json.load(f)
kernel.import_state(state)
```

### AtomSpace Compatibility

Kernel atoms map to OpenCog AtomSpace:

| Inferno Kernel | OpenCog AtomSpace |
|----------------|-------------------|
| Atom | Node |
| Link | Link |
| TruthValue | SimpleTruthValue |
| AttentionValue | AttentionValue |
| Inference | LogicLink |

## Performance Optimization

### Kernel-Level Optimization

```python
# Use kernel for performance-critical operations
kernel = InfernoKernelBridge()
kernel.init()

# Bulk atom creation
atoms = []
for i in range(10000):
    tv = TruthValue(0.8, 0.7)
    atom = kernel.create_atom(f"atom_{i}", tv)
    atoms.append(atom)

# Parallel reasoning (in Inferno, would be concurrent)
inferences = []
for i in range(0, len(atoms), 2):
    if i + 1 < len(atoms):
        inf = kernel.deduce([atoms[i], atoms[i+1]])
        inferences.extend(inf)
```

### Distributed Performance

```python
# Distribute load across kernel instances
kernels = [InfernoKernelBridge() for _ in range(4)]
for k in kernels:
    k.init()

# Partition work
atoms_per_kernel = len(atoms) // len(kernels)
for i, kernel in enumerate(kernels):
    start = i * atoms_per_kernel
    end = start + atoms_per_kernel
    kernel_atoms = atoms[start:end]
    # Process in kernel...
```

## Testing Integration

### Unit Tests

```python
import unittest
from opencog.inferno_kernel.lib.inferno_bridge import (
    InfernoKernelBridge, TruthValue
)

class TestInfernoKernel(unittest.TestCase):
    def setUp(self):
        self.kernel = InfernoKernelBridge()
        self.kernel.init()
    
    def test_atom_creation(self):
        tv = TruthValue(0.9, 0.8)
        atom = self.kernel.create_atom("test", tv)
        self.assertEqual(atom.name, "test")
        self.assertEqual(atom.truth_value.strength, 0.9)
    
    def test_reasoning(self):
        tv = TruthValue(0.9, 0.8)
        a1 = self.kernel.create_atom("A", tv)
        a2 = self.kernel.create_atom("B", tv)
        inferences = self.kernel.deduce([a1, a2])
        self.assertGreater(len(inferences), 0)
```

### Integration Tests

```python
def test_full_integration():
    """Test complete pipeline."""
    # Initialize all components
    from opencog.lib.opencog_analyzer import OpenCogAnalyzer
    from opencog.lib.hypergraph_analyzer import HypergraphAnalyzer
    from opencog.inferno_kernel.lib.inferno_bridge import InfernoKernelBridge
    
    analyzer = OpenCogAnalyzer('.')
    hypergraph = HypergraphAnalyzer('.')
    kernel = InfernoKernelBridge()
    kernel.init()
    
    # Process data through pipeline
    languages = analyzer.get_all_languages()[:10]
    for lang in languages:
        tv = TruthValue(0.8, 0.7)
        kernel.create_atom(f"lang_{lang}", tv)
    
    # Verify integration
    state = kernel.export_state()
    assert state['statistics']['total_atoms'] >= 10
```

## Deployment

### Development Deployment

```bash
# Build kernel
cd opencog/inferno-kernel
mk all

# Run tests
mk test

# Run examples
mk run-selfaware
mk run-distributed
```

### Production Deployment

```bash
# Install kernel
cd opencog/inferno-kernel
mk install

# Configure systemd service (if applicable)
sudo systemctl enable cogkernel
sudo systemctl start cogkernel

# Monitor
cogstat
cogmon
```

### Docker Deployment

```dockerfile
FROM infernoorg/inferno:latest

COPY opencog/inferno-kernel /opt/cogkernel
WORKDIR /opt/cogkernel

RUN mk all && mk install

EXPOSE 9999

CMD ["/dis/cogkernel/cogboot.dis"]
```

## Troubleshooting

### Common Issues

**Issue**: Kernel fails to initialize
```python
# Solution: Check Inferno environment
import os
inferno_root = os.getenv('INFERNO_ROOT')
if not inferno_root:
    print("Set INFERNO_ROOT environment variable")
```

**Issue**: Bridge communication errors
```python
# Solution: Verify bridge is using correct protocol
bridge = InfernoKernelBridge()
status = bridge.init()
print(f"Status: {status}")
```

**Issue**: 9P mount fails
```bash
# Solution: Check network connectivity
ping remotehost
telnet remotehost 9P
```

## Best Practices

1. **Initialize Once**: Create kernel bridge once and reuse
2. **Batch Operations**: Group atom creations for efficiency
3. **Export Regularly**: Save cognitive state periodically
4. **Monitor Attention**: Track STI budget and allocation
5. **Distribute Load**: Use 9P for multi-node deployment
6. **Type Safety**: Leverage Limbo's type system when possible
7. **Test Thoroughly**: Unit test all kernel interactions

## Future Enhancements

- Real-time streaming of cognitive events
- WebSocket bridge for browser integration
- REST API for HTTP access
- GraphQL interface for complex queries
- Prometheus metrics export
- Distributed tracing integration
- Hot-reload of kernel modules
- REPL for interactive development

## Conclusion

The Inferno Cognitive Kernel provides a revolutionary foundation for AGI development by making cognition a fundamental operating system service. This integration guide demonstrates how to leverage this architecture within the RosettaCog ecosystem while maintaining compatibility with existing OpenCog components.
