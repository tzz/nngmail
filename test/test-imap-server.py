"""Minimal Gmail-flavoured IMAP server for nngmail import tests.
N messages with sparse UIDs (every 3rd UID missing), X-GM-* attributes,
literal header blocks. Plain TCP; the client is told to use 'network'."""
import socketserver, sys, re, time
N = int(sys.argv[1]) if len(sys.argv) > 1 else 5000
DELAY = float(sys.argv[2]) if len(sys.argv) > 2 else 0.0
UIDS = [u for u in range(1, N * 3 // 2 + 1) if u % 3 != 0][:N]
UIDNEXT = UIDS[-1] + 1

def msg(uid):
    i = UIDS.index(uid)
    hdr = ("Subject: Message %d about =?UTF-8?Q?caf=C3=A9?=\r\n"
           "From: Sender %d <s%d@example.com>\r\n"
           "Date: Wed, 17 Sep 2026 10:%02d:00 -0400\r\n"
           "Message-ID: <m%d@example.com>\r\n"
           "References: <m%d@example.com>\r\n <m%d@example.com>\r\n"
           "\r\n") % (uid, i % 50, i % 50, i % 60, uid, max(1, uid-1), max(1, uid-2))
    labels = ['\\Inbox'] if i % 2 else ['"work/project"']
    if i % 7 == 0: labels.append('\\Starred')
    if i % 5 == 0: labels.append('"&AOk-t&AOk-"')  # été in UTF-7
    return ("* %d FETCH (X-GM-THRID %d X-GM-MSGID %d X-GM-LABELS (%s) UID %d RFC822.SIZE %d "
            "BODY[HEADER.FIELDS (Subject From Date Message-ID References In-Reply-To To Cc Newsgroups)] {%d}\r\n%s)\r\n"
            % (i+1, 1000000 + uid // 4, 5000000000000000000 + uid, " ".join(labels), uid, 1000 + uid, len(hdr), hdr))

class H(socketserver.StreamRequestHandler):
    def send(self, s):
        b = s.encode()
        if RATE:
            for i in range(0, len(b), 4096):
                self.wfile.write(b[i:i+4096]); self.wfile.flush(); time.sleep(4096 / RATE)
        else:
            self.wfile.write(b); self.wfile.flush()
    def handle(self):
        self.send("* OK Gimap ready\r\n")
        while True:
            line = self.rfile.readline().decode(errors="replace")
            if not line: return
            m = re.match(r"(\S+) (\S+)(.*)", line.strip())
            if not m: continue
            tag, cmd, rest = m.groups(); cmd = cmd.upper()
            if cmd == "CAPABILITY":
                self.send("* CAPABILITY IMAP4rev1 AUTH=XOAUTH2 IDLE X-GM-EXT-1 UIDPLUS\r\n%s OK done\r\n" % tag)
            elif cmd == "AUTHENTICATE":
                if "disabled" in DISABLED:
                    self.send("%s NO [ALERT] IMAP access is disabled for your domain.\r\n" % tag)
                else:
                    self.send("* CAPABILITY IMAP4rev1 IDLE X-GM-EXT-1\r\n%s OK success\r\n" % tag)
            elif cmd == "EXAMINE":
                self.send("* %d EXISTS\r\n* OK [UIDVALIDITY 1] UIDs valid\r\n* OK [UIDNEXT %d] Predicted next UID\r\n%s OK [READ-ONLY] examined\r\n" % (N, UIDNEXT, tag))
            elif cmd == "UID":
                mm = re.match(r"\s*FETCH (\d+):(\d+)", rest)
                lo, hi = int(mm.group(1)), int(mm.group(2))
                if DELAY: time.sleep(DELAY)
                for u in UIDS:
                    if lo <= u <= hi:
                        self.send(msg(u))
                self.send("%s OK Success\r\n" % tag)
            elif cmd == "LOGOUT":
                self.send("* BYE\r\n%s OK\r\n" % tag); return
            else:
                self.send("%s BAD unknown\r\n" % tag)
DISABLED = sys.argv[3] if len(sys.argv) > 3 else ""
RATE = float(sys.argv[4]) if len(sys.argv) > 4 else 0  # bytes/s per connection, 0 = unlimited
socketserver.TCPServer.allow_reuse_address = True
socketserver.ThreadingTCPServer(("127.0.0.1", 1993), H).serve_forever()
