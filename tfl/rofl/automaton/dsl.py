from collections import defaultdict
import sys


def generate(text):
    states = set()
    initals = set()
    finals = set()
    transitions = defaultdict(lambda: defaultdict(set))
    alphabet = set()

    for line in text.strip().split("\n"):
        line = line.strip()

        if not line or line == "NFA":
            continue

        parts = line.split()

        if parts[0].isdigit() and ";" in line and "label" in line:
            state = int(parts[0])
            states |= {state}

            if "initial" in line:
                initals |= {state}

            if "final" in line:
                finals |= {state}

        elif len(parts) >= 3 and parts[0].isdigit() and parts[1].isdigit():
            u = int(parts[0])
            v = int(parts[1])
            symbol = parts[2]

            states |= {u, v}
            alphabet.add(symbol)
            transitions[u][symbol] |= {v}

    states = sorted(states)
    alphabet = sorted(alphabet)
    initals = sorted(initals)
    finals = sorted(finals)

    transitions_str = ",\n".join(
        f"\t({u}, '{symbol}') => HashSet::from({sorted(val2)})"
        for u, val in sorted(transitions.items())
        for symbol, val2 in sorted(val.items())
    )

    return f"""let mut nfa = Nfa::with_transitions(
    HashSet::from({states}),
    HashSet::from_iter("{''.join(alphabet)}".chars()),
    HashSet::from({finals}),
    HashSet::from({initals}),
    map![
{transitions_str}
    ],
);"""


def main():
    if len(sys.argv) > 1:
        with open(sys.argv[1], "r") as f:
            text = f.read()
    else:
        text = sys.stdin.read()

    print(generate(text))


if __name__ == "__main__":
    main()
