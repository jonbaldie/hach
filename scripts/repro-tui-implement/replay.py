"""Replay a live /implement TUI workflow. Requires pyte; reads key without logging it.
Usage: python replay.py [default|dontAsk] [run-name]
"""
import os, sys, json, time, pty, select, subprocess, fcntl, termios, struct, codecs, tempfile
from pathlib import Path
import pyte
ROOT=Path(__file__).resolve().parent
REPO=ROOT.parents[1]
ARTIFACTS=Path(os.environ.get('HACH_PROBE_OUTPUT', str(Path(tempfile.gettempdir())/'hach-implement-investigation'))).resolve()
ARTIFACTS.mkdir(parents=True, exist_ok=True)
subprocess.run(['bash', str(ROOT/'build.sh'), str(ARTIFACTS)], cwd=REPO, check=True)
mode=sys.argv[1] if len(sys.argv)>1 else 'default'
name=sys.argv[2] if len(sys.argv)>2 else str(int(time.time()))
run=ARTIFACTS/name
run.mkdir() # never reuse state
project=run/'project'; project.mkdir()
wire=run/'wire'; wire.mkdir()
(project/'.agents').mkdir()
(project/'.agents/settings.json').write_text(json.dumps({'effort_level':'high','permission_mode':mode}))
(project/'.gitignore').write_text('__pycache__/\n')
(project/'AGENTS.md').write_text('This is an isolated Python fixture. All work must stay within this project. Implement directly using the existing public function as the seam. Run python3 -m unittest -v. Review the diff yourself; no delegation, network tools, or publishing. Commit the result locally.\n')
(project/'numbers.py').write_text('def clamp(value, lower, upper):\n    raise NotImplementedError\n')
(project/'test_numbers.py').write_text('''import unittest
from numbers import clamp
class ClampTests(unittest.TestCase):
    def test_inside(self): self.assertEqual(clamp(5, 0, 10), 5)
    def test_below(self): self.assertEqual(clamp(-2, 0, 10), 0)
    def test_above(self): self.assertEqual(clamp(12, 0, 10), 10)
    def test_equal_bounds(self): self.assertEqual(clamp(5, 3, 3), 3)
    def test_invalid_bounds(self):
        with self.assertRaises(ValueError): clamp(5, 10, 0)
''')
def cmd(args):
    r=subprocess.run(args,cwd=project,text=True,capture_output=True)
    return {'code':r.returncode,'stdout':r.stdout,'stderr':r.stderr}
for args in [['git','init','-q'],['git','config','user.name','TUI Fixture'],['git','config','user.email','fixture@example.invalid'],['git','add','.'],['git','commit','-qm','Initial fixture']]:
    assert cmd(args)['code']==0
reset={'git':cmd(['git','status','--porcelain']),'tests':cmd(['python3','-m','unittest','-v']),'source':(project/'numbers.py').read_text()}
assert reset['git']['stdout']=='' and reset['tests']['code']!=0
(run/'reset.json').write_text(json.dumps(reset,indent=2))
env=os.environ.copy()
if not env.get('OPENROUTER_API_KEY'):
    for line in (REPO/'.env').read_text().splitlines():
        if line.startswith('OPENROUTER_API_KEY='): env['OPENROUTER_API_KEY']=line.split('=',1)[1].strip().strip('\"\'')
assert env.get('OPENROUTER_API_KEY')
env.update(TERM='xterm-256color',HACH_PROBE_DIR=str(wire),CLAUDE_CONFIG_DIR=str(project/'isolated-user-config'))
master,slave=pty.openpty()
fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',40,120,0,0))
p=subprocess.Popen([str(ARTIFACTS/'hach-probe'),'--model','openai/gpt-5.6-luna','--max-turns',os.environ.get('HACH_PROBE_MAX_TURNS','12')],cwd=project,env=env,stdin=slave,stdout=slave,stderr=slave)
os.close(slave)
screen=pyte.Screen(120,40); stream=pyte.Stream(screen); decoder=codecs.getincrementaldecoder('utf8')('replace')
raw=open(run/'terminal.log','wb')
def drain(seconds):
    end=time.monotonic()+seconds
    while time.monotonic()<end:
        if select.select([master],[],[],min(.2,max(0,end-time.monotonic())))[0]:
            try: data=os.read(master,65536)
            except OSError: break
            if not data: break
            raw.write(data); raw.flush(); stream.feed(decoder.decode(data))
def probe(action):
    display='\n'.join(screen.display)
    (run/f'{action}-screen.txt').write_text(display)
    result={'action':action,'process_alive':p.poll() is None,'source':(project/'numbers.py').read_text(),'git':cmd(['git','status','--porcelain']),'requests':len(list(wire.glob('*-original.json'))),'responses':len(list(wire.glob('*-response.json'))),'hello_exists':(project/'hello.txt').exists()}
    with open(run/'probes.jsonl','a') as out: out.write(json.dumps(result)+'\n')
    return display
try:
    drain(3)
    launch=probe('launch')
    assert p.poll() is None and '● idle' in launch and 'openai/gpt-5.6-luna' in launch, 'TUI startup probe failed; inspect launch-screen.txt'
    prompt='/implement Implement clamp(value, lower, upper) in numbers.py: return value within inclusive bounds, clamp outside values to the nearest bound, and raise ValueError if lower > upper. Existing tests define the agreed seam. Run them, review your change, and commit locally.'
    prompt=os.environ.get('HACH_PROBE_PROMPT',prompt)
    (run/'actions.json').write_text(json.dumps([{'action':'submit','text':prompt},{'action':'wait_idle','timeout_seconds':240},{'action':'wheel_up'},{'action':'wheel_down'},{'action':'quit'}],indent=2))
    os.write(master,prompt.encode()); drain(.5); probe('typed'); os.write(master,b'\r'); drain(2); probe('submit')
    start=time.monotonic(); last_report=0
    while time.monotonic()-start<240 and p.poll() is None:
        drain(1)
        display='\n'.join(screen.display)
        responses=list(wire.glob('*-response.json'))
        if time.monotonic()-last_report>20:
            print(json.dumps({'run':name,'elapsed':round(time.monotonic()-start),'requests':len(list(wire.glob('*-original.json'))),'responses':len(responses)}),flush=True); last_report=time.monotonic()
        if responses and ('● idle' in display or '✔ ready' in display or '✖ error:' in display): break
    probe('completed')
    os.write(master,b'\x1b[<64;10;10M'); drain(.4); probe('wheel-up')
    os.write(master,b'\x1b[<65;10;10M'); drain(.4); probe('wheel-down')
    result={'tests':cmd(['python3','-m','unittest','-v']),'log':cmd(['git','log','--oneline','-3']),'source':(project/'numbers.py').read_text(),'timed_out':time.monotonic()-start>=240}
    original=[json.loads(f.read_text()) for f in wire.glob('*-original.json')]
    sent=[json.loads(f.read_text()) for f in wire.glob('*-wire.json')]
    tool_messages=[m for d in original for m in d['messages'] if m['role']=='tool']
    result['observations']={
        'skill_expanded':any('<skill name="implement">' in str(d['messages']) for d in original),
        'configured_effort_omitted':any('reasoning' not in d and 'reasoning_effort' not in d for d in original),
        'all_transmissions_luna_high':bool(sent) and all(d['model']=='openai/gpt-5.6-luna' and d['reasoning']['effort']=='high' for d in sent),
        'permission_denial_observed':any('Execution denied by permission policy.' in m['content'] for m in tool_messages) or 'Permission denied by policy' in '\n'.join(screen.display),
        'hello_exists':(project/'hello.txt').exists(),
        'fixture_unchanged':(project/'numbers.py').read_text()==reset['source'],
    }
    (run/'result.json').write_text(json.dumps(result,indent=2))
    os.write(master,b'\x11'); drain(1); probe('quit')
    print(json.dumps({'run':str(run),'tests_exit':result['tests']['code'],'timed_out':result['timed_out'],'observations':result['observations']},indent=2),flush=True)
finally:
    if p.poll() is None: p.terminate()
    p.wait(timeout=10); raw.close(); os.close(master)
