import re, math

# --- Functions & constants allowed ---
FUNCTIONS = {
    'sqrt': (1, lambda x: math.sqrt(x)),
    'sin':  (1, lambda x: math.sin(x)),
    'cos':  (1, lambda x: math.cos(x)),
    'tan':  (1, lambda x: math.tan(x)),
    'abs':  (1, lambda x: abs(x)),
    'ln':   (1, lambda x: math.log(x)),
    'log':  (1, lambda x: math.log10(x)),
}
CONSTANTS = {'pi': math.pi, 'e': math.e}

# --- Operator metadata (precedence & associativity) ---
precedence = {'u-': 5, '^': 4, '*': 3, '/': 3, '+': 2, '-': 2}
assoc      = {'u-': 'right', '^': 'right', '*': 'left', '/': 'left', '+': 'left', '-': 'left'}

# --- Tokenizer (handles spaces, numbers, names, operators, (), =, and unary -) ---
_token_body = re.compile(r'([A-Za-z_]\w*|\d*\.\d+|\d+|==|!=|<=|>=|[+\-*/^(),=])')
def tokenize(expr: str):
    tokens, i, n = [], 0, len(expr)
    while i < n:
        while i < n and expr[i].isspace(): i += 1
        if i >= n: break
        m = _token_body.match(expr, i)
        if not m: raise ValueError(f"Unexpected character at position {i}: {expr[i]}")
        tokens.append(m.group(1)); i = m.end()
    # patch unary minus
    fixed, prev = [], None
    for t in tokens:
        if t == '-' and (prev is None or prev in '()+-*/^=,'):
            fixed.append('u-')
        else:
            fixed.append(t)
        prev = t
    return fixed

# Insert implicit multiplication: 2x, (x+1)(x-1), 3(x+2), pi x, etc.
def insert_implicit_mult(tokens):
    def is_operand(tok):
        return tok == 'x' or tok in CONSTANTS or re.fullmatch(r'\d*\.\d+|\d+', tok) or tok == ')'
    def can_follow(tok):
        return tok in FUNCTIONS or tok == '(' or tok == 'x' or tok in CONSTANTS or re.fullmatch(r'\d*\.\d+|\d+', tok)
    out = []
    for t in tokens:
        if out:
            prev = out[-1]
            if (is_operand(prev) or prev == ')') and can_follow(t) and (t not in precedence) and t not in {',',')','='}:
                out.append('*')
        out.append(t)
    return out

# --- Shunting-yard: infix -> RPN (postfix) with step logging ---
def to_rpn(tokens, verbose=False):
    out, stack, steps = [], [], []
    def log(action, tok=None):
        if verbose:
            steps.append({'token': tok, 'action': action, 'stack': stack.copy(), 'output': out.copy()})
    for t in tokens:
        if re.fullmatch(r'\d*\.\d+|\d+', t): out.append(float(t)); log("push number", t)
        elif t in CONSTANTS: out.append(float(CONSTANTS[t])); log(f"push const {t}", t)
        elif t == 'x': out.append('x'); log("push variable x", t)
        elif t in FUNCTIONS: stack.append(t); log("push function", t)
        elif t == ',':
            while stack and stack[-1] != '(': out.append(stack.pop())
            log("comma: pop until '('", t)
        elif t in precedence:  # operator
            while stack and stack[-1] != '(':
                top = stack[-1]
                if top in FUNCTIONS:
                    out.append(stack.pop()); log("pop function", t)
                elif (top in precedence and
                      ((assoc.get(t,'left') == 'left' and precedence[top] >= precedence[t]) or
                       (assoc.get(t,'left') == 'right' and precedence[top] >  precedence[t]))):
                    out.append(stack.pop()); log("pop operator", t)
                else:
                    break
            stack.append(t); log("push operator", t)
        elif t == '(':
            stack.append(t); log("push '('", t)
        elif t == ')':
            while stack and stack[-1] != '(': out.append(stack.pop()); log("pop until '('", t)
            if not stack: raise ValueError("Mismatched parentheses")
            stack.pop(); log("discard '('", t)
            if stack and stack[-1] in FUNCTIONS: out.append(stack.pop()); log("pop function after ')'", t)
        elif t == '=':
            out.append('='); log("equals", t)
        else:
            raise ValueError(f"Unknown token {t}")
    while stack:
        if stack[-1] in '()': raise ValueError("Mismatched parentheses")
        out.append(stack.pop()); log("drain stack")
    return out, steps

# --- Evaluate numeric RPN with step logging ---
def eval_rpn(rpn, verbose=False):
    stack, steps = [], []
    def log(msg): 
        if verbose: steps.append({'op': msg, 'stack': stack.copy()})
    for token in rpn:
        if isinstance(token, float):
            stack.append(token); log(f"push {token}")
        elif token in FUNCTIONS:
            arity, func = FUNCTIONS[token]
            if len(stack) < arity: raise ValueError("Insufficient operands")
            args = [stack.pop() for _ in range(arity)][::-1]
            res = func(*args)
            stack.append(res); log(f"{token}({', '.join(map(str,args))}) -> {res}")
        elif token in ('u-','+','-','*','/','^'):
            if token == 'u-':
                if not stack: raise ValueError("Insufficient operands")
                a = stack.pop(); stack.append(-a); log(f"negate -> {-a}")
            else:
                if len(stack) < 2: raise ValueError("Insufficient operands")
                b, a = stack.pop(), stack.pop()
                res = {'+': a+b, '-': a-b, '*': a*b, '/': a/b, '^': a**b}[token]
                stack.append(res); log(f"{a} {token} {b} -> {res}")
        elif token == 'x':
            raise ValueError("Found variable x; this is an equation, not a pure numeric expression.")
        elif token == '=':
            raise ValueError("Unexpected '=' in numeric evaluation.")
        else:
            raise ValueError(f"Unknown token {token}")
    if len(stack) != 1: raise ValueError("Invalid expression")
    return stack[0], steps

# --- Linear (in x) evaluator on RPN using dual numbers: value = A + B*x ---
def eval_linear_rpn(rpn, verbose=False):
    stack, logs = [], []
    def push(pair, desc):
        stack.append(pair)
        if verbose: logs.append(f"push {desc}: {pair}; stack={stack}")
    def binop(op, A, B):
        (a0,a1),(b0,b1)=A,B
        if op=='+':  return (a0+b0, a1+b1), f"({a0}+{b0}) + ({a1}+{b1})x"
        if op=='-':  return (a0-b0, a1-b1), f"({a0}-{b0}) + ({a1}-{b1})x"
        if op=='*':
            if a1!=0 and b1!=0: raise ValueError("Non-linear term x*x encountered")
            return (a0*b0, a0*b1 + a1*b0), f"({a0}*{b0}) + ({a0}*{b1}+{a1}*{b0})x"
        if op=='/':
            if b1!=0: raise ValueError("Division by expression containing x not supported")
            return (a0/b0, a1/b0), f"({a0}/{b0}) + ({a1}/{b0})x"
        if op=='^':
            if b1!=0 or a1!=0 and b0!=1: raise ValueError("Non-linear power encountered")
            return (a0**b0, a1 * (b0*(a0**(b0-1)) if a1!=0 else 0)), f"{a0}^{b0}"
        raise ValueError("Unsupported operator")
    for t in rpn:
        if isinstance(t, float): push((t,0.0), f"number {t}")
        elif t=='x':            push((0.0,1.0), "variable x")
        elif t=='u-':
            a0,a1 = stack.pop(); push((-a0,-a1), "negate")
        elif t in FUNCTIONS:
            a0,a1 = stack.pop()
            if a1!=0: raise ValueError("Function applied to expression containing x not supported")
            arity, func = FUNCTIONS[t]; res = func(a0)
            push((res,0.0), f"{t}({a0})")
        elif t in ('+','-','*','/','^'):
            b, a = stack.pop(), stack.pop()
            res,desc = binop(t, a, b); push(res, f"{a} {t} {b} -> {desc}")
        elif t == '=':
            raise ValueError("Unexpected '=' in RPN")
        else:
            raise ValueError(f"Unknown token {t}")
    if len(stack)!=1: raise ValueError("Invalid linear RPN")
    return stack[0], logs

def solve_linear(eq, verbose=False):
    left, right = eq.split('=', 1)
    ltoks = insert_implicit_mult(tokenize(left))
    rtoks = insert_implicit_mult(tokenize(right))
    lrpn,_ = to_rpn(ltoks, verbose=False)
    rrpn,_ = to_rpn(rtoks, verbose=False)
    L, llog = eval_linear_rpn(lrpn, verbose=verbose)
    R, rlog = eval_linear_rpn(rrpn, verbose=verbose)
    A, B = (L[0]-R[0]), (L[1]-R[1])
    steps = []
    if verbose:
        steps.append(f"LHS as A+Bx: {L}"); steps += llog
        steps.append(f"RHS as A+Bx: {R}"); steps += rlog
        steps.append(f"Bring to one side: (A_L-A_R, B_L-B_R)=({A}, {B})  ->  Bx + A = 0")
    if abs(B) < 1e-12:
        return ("Infinite solutions" if abs(A) < 1e-12 else "No solution"), steps
    x = -A/B
    steps.append(f"Solve Bx + A = 0  =>  x = -A/B = {-A}/{B} = {x}")
    return x, steps

def explain(expr):
    if '=' in expr:
        result, steps = solve_linear(expr, verbose=True)
        return {'type':'equation','result':result,'steps':steps}
    tokens = insert_implicit_mult(tokenize(expr))
    rpn, ssteps = to_rpn(tokens, verbose=True)
    value, esteps = eval_rpn(rpn, verbose=True)
    return {
        'type':'expression', 'tokens':tokens, 'rpn':rpn,
        'infix_to_rpn_steps': ssteps, 'rpn_eval_steps': esteps, 'result': value
    }

# --- CLI ---
def main():
    print("✍️  Handwritten Math Evaluator — Step-by-Step")
    print(" • Arithmetic: e.g., 2 + 3*(4-1), 2^3 + sqrt(16)")
    print(" • Linear equations in x: e.g., 4x + 2 = 18, 3(2x-4)=6")
    print("Commands: steps on | steps off | help | exit\n")
    show_steps = True
    while True:
        s = input(">>> ").strip()
        if not s: continue
        if s.lower() in {'exit','quit','q'}: print("Bye!"); break
        if s.lower() == 'help':
            print("Allowed ops: +, -, *, /, ^, parentheses, implicit multiplication (e.g., 3x or (x+1)(x-1)).")
            print("Functions: sqrt, sin, cos, tan, abs, ln, log; constants: pi, e.")
            print("Equations: linear in x only (no x in denominators/exponents)."); continue
        if s.lower() == 'steps on':  show_steps = True;  print("Step-by-step: ON"); continue
        if s.lower() == 'steps off': show_steps = False; print("Step-by-step: OFF"); continue
        try:
            info = explain(s)
            if info['type'] == 'expression':
                print(f"\nResult: {info['result']}")
                if show_steps:
                    print("\nTokens:", info['tokens'])
                    print("Postfix (RPN):", info['rpn'])
                    print("\nInfix → RPN steps:")
                    for i, st in enumerate(info['infix_to_rpn_steps'], 1):
                        print(f"  {i:02d}. token={st['token']!r:>6} | {st['action']:<22} | stack={st['stack']} | output={st['output']}")
                    print("\nRPN evaluation steps:")
                    for i, st in enumerate(info['rpn_eval_steps'], 1):
                        print(f"  {i:02d}. {st['op']:<28} | stack={st['stack']}")
                print()
            else:
                print(f"\nSolution for x: {info['result']}")
                if show_steps:
                    print("\nSteps:")
                    for i, line in enumerate(info['steps'], 1):
                        print(f"  {i:02d}. {line}")
                print()
        except Exception as e:
            print("Error:", e)

if __name__ == "__main__":
    main()
