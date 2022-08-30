unit MiTeC_FPC;

{$MODE Delphi}

interface

uses Windows, Classes;

function AllocateHWnd(const AMethod: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

implementation

type
  PObjectInstance = ^TObjectInstance;
  TObjectInstance = packed record
    Code: Byte;
    Offset: Integer;
    case Integer of
      0: (Next: PObjectInstance);
      1: (FMethod: TMethod);
  end;

const
{$IF Defined(CPUX86)}
  CodeBytes = 2;
{$ELSEIF Defined(CPUX64)}
  CodeBytes = 8;
{$ENDIF CPU}
  InstanceCount = (4096 - SizeOf(Pointer) * 2 - CodeBytes) div SizeOf(TObjectInstance) - 1;

type
  PInstanceBlock = ^TInstanceBlock;
  TInstanceBlock = packed record
    Next: PInstanceBlock;
    Code: array[1..CodeBytes] of Byte;
    WndProcPtr: Pointer;
    Instances: array[0..InstanceCount] of TObjectInstance;
  end;

var
  InstBlockList: PInstanceBlock;
  InstFreeList: PObjectInstance;

{$ASMMODE intel}
function StdWndProc(Window: HWND; Message: UINT; WParam: WPARAM; LParam: WPARAM): LRESULT; stdcall;
  {$IF Defined(CPUX86)}
  { In    ECX = Address of method pointer }
  { Out   EAX = Result }
  asm
          XOR     EAX,EAX
          PUSH    EAX
          PUSH    LParam
          PUSH    WParam
          PUSH    Message
          MOV     EDX,ESP
          MOV     EAX,[ECX].Longint[4]
          CALL    [ECX].Pointer
          ADD     ESP,12
          POP     EAX
  end;
  {$ELSEIF Defined(CPUX64)}
  { In    R11 = Address of method pointer }
  { Out   RAX = Result }
  var
    Msg: TMessage;
  asm
          MOV     Msg.Msg,Message
          MOV     Msg.WParam,WParam
          MOV     Msg.LParam,LParam
          MOV     Msg.Result,0
          LEA     RDX,Msg
          MOV     RCX,[R11].TMethod.Data
          CALL    [R11].TMethod.Code
          MOV     RAX,Msg.Result
  end;
  {$ENDIF CPUX64}

function CalcJmpOffset(Src, Dest: Pointer): Longint;
begin
  Result := IntPtr(Dest) - (IntPtr(Src) + 5);
end;

function MakeObjectInstance(const AMethod: TWndMethod): Pointer;
const
  BlockCode: array[1..CodeBytes] of Byte = (
{$IF Defined(CPUX86)}
    $59,                       { POP ECX }
    $E9);                      { JMP StdWndProc }
{$ELSEIF Defined(CPUX64)}
    $41,$5b,                   { POP R11 }
    $FF,$25,$00,$00,$00,$00);  { JMP [RIP+0] }
{$ENDIF}
  PageSize = 4096;
var
  Block: PInstanceBlock;
  Instance: PObjectInstance;
begin
  if InstFreeList = nil then
  begin
    Block := VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Block^.Next := InstBlockList;
    Move(BlockCode, Block^.Code, SizeOf(BlockCode));
{$IF Defined(CPUX86)}
    Block^.WndProcPtr := Pointer(CalcJmpOffset(@Block^.Code[2], @StdWndProc));
{$ELSEIF Defined(CPUX64)}
    Block^.WndProcPtr := @StdWndProc;
{$ENDIF}
    Instance := @Block^.Instances;
    repeat
      Instance^.Code := $E8;  { CALL NEAR PTR Offset }
      Instance^.Offset := CalcJmpOffset(Instance, @Block^.Code);
      Instance^.Next := InstFreeList;
      InstFreeList := Instance;
      Inc(PByte(Instance), SizeOf(TObjectInstance));
    until IntPtr(Instance) - IntPtr(Block) >= SizeOf(TInstanceBlock);
    InstBlockList := Block;
  end;
  Result := InstFreeList;
  Instance := InstFreeList;
  InstFreeList := Instance^.Next;
  Instance^.FMethod := TMethod(AMethod);
end;

{ Free an object instance }

procedure FreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then
  begin
    PObjectInstance(ObjectInstance)^.Next := InstFreeList;
    InstFreeList := ObjectInstance;
  end;
end;

var
  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindow');

function AllocateHWnd(const AMethod: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (@TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
    '', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(AMethod) then
    SetWindowLongPtr(Result, GWL_WNDPROC, IntPtr(MakeObjectInstance(AMethod)));
end;

procedure DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLongPtr(Wnd, GWL_WNDPROC));
  DestroyWindow(Wnd);
  if Instance <> @DefWindowProc then FreeObjectInstance(Instance);
end;

end.
