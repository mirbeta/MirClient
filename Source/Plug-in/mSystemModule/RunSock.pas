unit RunSock;

interface
uses
  Grobal2;
type
  TRunSocket = class
  private
  public
    procedure Run();
  end;
var
  RunSocket                 : TRunSocket;
  SocketRun                 : TRunSocketRun;
implementation

uses Module;

procedure TRunSocket.Run;
begin
  if Assigned(SocketRun) then
    SocketRun(Self);
end;

end.

 