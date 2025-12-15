# CLAUDE.md - Project Guide for AI Assistants

## Project Overview

**ros9etta** (RosettaCog) is a post-polyglot meta-intelligence framework that analyzes 970+ programming languages across 1,228 computational tasks from RosettaCode.org. The project evaluates language capabilities for AI/AGI applications and enables automated multi-language synthesis (FrankenCog).

## Repository Structure

```
ros9etta/
├── Lang/              # 970 programming languages organized by name (~27K+ code samples)
├── Task/              # 1,228 computational tasks with multi-language implementations
├── opencog/           # AI/AGI evaluation framework (Python)
│   ├── lib/           # Core Python modules (analyzers, hypergraph, atom types)
│   ├── bin/           # CLI tools for analysis
│   ├── agents/        # Multi-agent reasoning system (Agent-Zero)
│   ├── patterns/      # 10 cognitive patterns for AGI
│   ├── strategies/    # 7 reasoning strategies
│   ├── reasoning-tasks/  # 95 LLM reasoning challenges
│   ├── inferno-kernel/   # Inferno OS cognitive kernel implementation
│   └── data/          # Task categorization mappings (JSON)
├── bin/               # RosettaCode data tools (Perl/Bash)
├── Conf/              # Language and task configuration (YAML)
├── agents/            # Agent definition files
├── Analysis/          # Analysis output reports
└── Repo/              # Repository metadata
```

## Tech Stack

- **Python 3**: Core analysis framework (`opencog/lib/*.py`)
- **Perl**: RosettaCode data collection (`cpanm RosettaCode`)
- **Bash**: CLI tool wrappers (`bin/`, `opencog/bin/`)
- **YAML**: Configuration files (`Conf/`)
- **JSON**: Data mappings (`opencog/data/`, `ext2lan.json`)

## Key Commands

### OpenCog Analysis Tools (from repo root)
```bash
opencog/bin/opencog-analyze              # Full language analysis
opencog/bin/opencog-hypergraph           # Hypergraph generation (45 subcategories)
opencog/bin/opencog-atom-types           # Formal atom type expressions
opencog/bin/opencog-reasoning            # Reasoning task analysis
opencog/bin/opencog-agent-zero <task> [mode]  # Multi-agent orchestration
opencog/bin/opencog-manifest             # FrankenCog integration manifest
opencog/bin/opencog-eval-lang <lang>     # Evaluate specific language
opencog/bin/opencog-eval-category <cat>  # Evaluate AI category
opencog/bin/opencog-report               # Generate full report
opencog/bin/opencog-inferno              # Inferno kernel interaction
```

### RosettaCode Data Tools
```bash
bin/rcd-api-list-all-langs    # List languages from rosettacode.org
bin/rcd-api-list-all-tasks    # List tasks from rosettacode.org
bin/rcd-samples-per-lang      # Code samples per language
bin/rcd-tasks-per-lang        # Tasks per language
bin/rcd-langs-per-task        # Languages per task
```

### Build Commands
```bash
make build    # Rebuild data using rosettacode Perl tool
make clean    # Remove Meta/ and logs
```

## AI/AGI Evaluation Categories

The framework evaluates languages across **10 cognitive domains** with **45 subcategories**:

1. **Symbolic Reasoning** - Logic, theorem proving, constraint satisfaction
2. **Pattern Recognition** - Search, matching, classification
3. **Knowledge Representation** - Data structures, graphs, semantic networks
4. **Machine Learning** - Statistical methods, optimization, neural networks
5. **Natural Language Processing** - Text analysis, parsing, NLP
6. **Planning & Problem Solving** - Heuristic search, game playing, puzzles
7. **Uncertainty Reasoning** - Probabilistic methods, fuzzy logic
8. **Cognitive Architecture** - Concurrent systems, agent-based systems
9. **Perception & Motor** - Image processing, signal processing
10. **Meta-Learning** - Self-improvement, reflection, code generation

## Key Python Modules

| Module | Purpose |
|--------|---------|
| `opencog/lib/opencog_analyzer.py` | Main language capability evaluator |
| `opencog/lib/hypergraph_analyzer.py` | Multi-dimensional relationship analysis |
| `opencog/lib/atom_types.py` | Formalized mathematical expression system |
| `opencog/lib/atom_type_builder.py` | Atom type construction framework |
| `opencog/workbench.py` | Multi-agent orchestration workbench |
| `opencog/opencog_reasoning_demo.py` | Reasoning task demonstration |

## Multi-Agent System

**Agent-Zero** orchestrates 5 specialized agents:
- **Analyzer**: Problem decomposition
- **Strategist**: Strategy selection
- **Executor**: Solution implementation
- **Validator**: Verification and testing
- **Learner**: Meta-learning and adaptation

## Development Notes

- Output files go to `opencog/output/` (gitignored)
- Python bytecode (`__pycache__/`, `*.pyc`) is gitignored
- Run Python scripts from repository root for proper path resolution
- CLI tools use `set -e -u -o pipefail` for strict error handling

## Important Files

- `ReadMe.md` - Project documentation and feature overview
- `ROADMAP.md` - 5-phase development roadmap (2025-2027+)
- `STRATEGIC_ANALYSIS.md` - Current state and gap analysis
- `opencog/README.md` - OpenCog framework documentation
- `opencog/HYPERGRAPH.md` - Subcategory taxonomy documentation
- `agents/rosettacog.md` - Agent definition and capabilities

## FrankenCog Concept

The project identifies the **optimal language for each AI function**:
- **C**: ML, NLP, Uncertainty, Perception (performance-critical)
- **Ada**: Concurrent systems, pattern recognition
- **C++**: Knowledge representation
- **C#**: Symbolic reasoning
- **11l**: Planning and problem solving
- **FreeBASIC**: Meta-learning and self-reflection

This enables "polyglot synthesis" where each component uses the best-suited language.
