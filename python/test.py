import sys


def mute_stdout():
    current_stdout = sys.stdout
    sys.stdout = open('/dev/null', 'w')
    return current_stdout

def unmute_stdout(handle):
    sys.stdout = handle

if __name__ == "__main__":
    print("hi")
    h = mute_stdout()
    print("x")
    unmute_stdout(h)
