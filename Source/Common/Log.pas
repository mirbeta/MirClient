unit Log;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.ComCtrls, Vcl.ExtCtrls;

type

  TConsoleColor = (
    BLACK,
    RED,
    GREEN,
    BROWN,
    BLUE,
    MAGENTA,
    CYAN,
    GREY,
    YELLOW,
    LRED,
    LGREEN,
    LBLUE,
    LMAGENTA,
    LCYAN,
    WHITE
  );

  TMemoLogMsg = packed record
    sMag: string;
    Color: TConsoleColor;
  end;
  pTMemoLogMsg = ^TMemoLogMsg;

  TLog = class(TObject)
  private
    FRichEditLog: TRichEdit;
    FMemoLogMsgList: TThreadList<TMemoLogMsg>;
    FTimer: TTimer;
    FdwShowMemoLogTick: LongWord;
    FShowLogLevel: Byte;   //��־�ȼ�
    procedure TimerEvent(Sender: TObject);
    procedure ShowMemoLogMsg();
    procedure setColor(stdout_stream: Boolean; Color: TConsoleColor);
    procedure ResetColor(stdout_stream: Boolean);
    //���뵽�߳��б�
    procedure AddThreadList(const FormatStr: string; Color: TConsoleColor); overload;
    procedure AddThreadList(const FormatStr: string; const Args: array of const; Color: TConsoleColor); overload;
    //����ַ���
    procedure outDefault(const FormatStr: string; Color: TConsoleColor);
  public
    constructor Create;
    destructor Destroy; override;
    //�ڳ�ʼ����ʱ��������ʾ����
    procedure setSource(Source: TRichEdit);
    procedure RichEditClear(value: Word);

    //ȫ����־����
    procedure outString(const FormatStr: string); overload;
    procedure outString(const FormatStr: string; const Args: array of const); overload;
    procedure outError(const FormatStr: string); overload;
    procedure outError(const FormatStr: string; const Args: array of const); overload;
    //��־�ȼ� >= 1  Ĭ�� һ�����ڻ�����Ϣ
    procedure outBasic(const FormatStr: string); overload;
    procedure outBasic(const FormatStr: string; const Args: array of const); overload;
    //��־�ȼ� >= 2  ϸ�� һ�����ڲ鿴��Ϣ
    procedure outDetail(const FormatStr: string); overload;
    procedure outDetail(const FormatStr: string; const Args: array of const); overload;
    //��־�ȼ� >= 3  ���� �������������Ϣ
    procedure outDebug(const FormatStr: string); overload;
    procedure outDebug(const FormatStr: string; const Args: array of const); overload;

    property ShowLogLevel: Byte read FShowLogLevel write FShowLogLevel;
  end;

var
  sLog: TLog;


implementation


{ TLog }

procedure TLog.RichEditClear(value: Word);
begin
  if not Assigned(FRichEditLog) then
    Exit;

  if FRichEditLog.Lines.Count > value then
    FRichEditLog.Clear;
end;

procedure TLog.AddThreadList(const FormatStr: string; const Args: array of const; Color: TConsoleColor);
var
  sFormat: string;
  MemoLogMsg: TMemoLogMsg;
begin
  if not Assigned(FRichEditLog) then
    Exit;

  sFormat := FormatStr;
  if sFormat = '' then
    Exit;

  try
    //��ʽ���ַ���
    sFormat := Format(sFormat, Args);
    MemoLogMsg.sMag := sFormat;
    MemoLogMsg.Color := Color;
    FMemoLogMsgList.Add(MemoLogMsg);
  except on E: Exception do
    begin
      sFormat := '�ַ�����ʽ������: ' + sFormat;
      MemoLogMsg.sMag := sFormat;
      MemoLogMsg.Color := Color;
      FMemoLogMsgList.Add(MemoLogMsg);
    end;
  end;
end;

procedure TLog.AddThreadList(const FormatStr: string; Color: TConsoleColor);
var
  sFormat: string;
  MemoLogMsg: TMemoLogMsg;
begin
  if not Assigned(FRichEditLog) then
    Exit;

  sFormat := FormatStr;
  if sFormat = '' then
    Exit;

  MemoLogMsg.sMag := sFormat;
  MemoLogMsg.Color := Color;
  FMemoLogMsgList.Add(MemoLogMsg);
end;

constructor TLog.Create;
begin
  inherited Create;
  FMemoLogMsgList := TThreadList<TMemoLogMsg>.Create;
  FdwShowMemoLogTick := GetTickCount();
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerEvent;
  FRichEditLog := nil;
end;

destructor TLog.Destroy;
begin
  FTimer.Free;
  FMemoLogMsgList.Free;
  inherited;
end;

procedure TLog.outBasic(const FormatStr: string; const Args: array of const);
begin
  if FShowLogLevel >= 1 then
    AddThreadList(FormatStr, Args, WHITE);
end;

procedure TLog.outBasic(const FormatStr: string);
begin
  if FShowLogLevel >= 1 then
    AddThreadList(FormatStr, WHITE);
end;

procedure TLog.outDebug(const FormatStr: string);
begin
  if FShowLogLevel >= 3 then
    AddThreadList(FormatStr, RED);
end;

procedure TLog.outDebug(const FormatStr: string; const Args: array of const);
begin
  if FShowLogLevel >= 3 then
    AddThreadList(FormatStr, Args, RED);
end;

procedure TLog.outDefault(const FormatStr: string; Color: TConsoleColor);
var
  sFormat: string;
  nLineCount: Integer;
begin
  if not Assigned(FRichEditLog) then
    Exit;

  sFormat := FormatStr;
  if sFormat = '' then
    Exit;

  //����ƶ���ĩβ
  nLineCount := FRichEditLog.Lines.Count;
  FRichEditLog.SelStart := SendMessage(FRichEditLog.Handle, EM_LINEINDEX, nLineCount, 0);
  setColor(True, Color);
  try
    FRichEditLog.Lines.Add(sFormat);
  except

  end;
  ResetColor(True);

  SendMessage(FRichEditLog.Handle, WM_VSCROLL, SB_BOTTOM, 0); //���Ϲ���)
end;

procedure TLog.outDetail(const FormatStr: string; const Args: array of const);
begin
  if FShowLogLevel >= 2 then
    AddThreadList(FormatStr, Args, LRED);
end;

procedure TLog.outDetail(const FormatStr: string);
begin
  if FShowLogLevel >= 2 then
    AddThreadList(FormatStr, LRED);
end;

procedure TLog.outError(const FormatStr: string; const Args: array of const);
begin
  AddThreadList(FormatStr, Args, RED);
end;

procedure TLog.outError(const FormatStr: string);
begin
  AddThreadList(FormatStr, RED);
end;

procedure TLog.outString(const FormatStr: string; const Args: array of const);
begin
  AddThreadList(FormatStr, Args, GREEN);
end;

procedure TLog.outString(const FormatStr: string);
begin
  AddThreadList(FormatStr, GREEN);
end;

procedure TLog.ResetColor(stdout_stream: Boolean);
begin
  FRichEditLog.SelAttributes.Color := clWhite;
end;

procedure TLog.setColor(stdout_stream: Boolean; Color: TConsoleColor);
begin
  if not Assigned(FRichEditLog) then
    Exit;

  case Color of
    BLACK:
      FRichEditLog.SelAttributes.Color := clBlack;                 //��ɫ
    RED:
      FRichEditLog.SelAttributes.Color := clRed;                   //��ɫ
    GREEN:
      FRichEditLog.SelAttributes.Color := clLime;                  //��ɫ
    BROWN:
      FRichEditLog.SelAttributes.Color := clWebSaddleBrown;        //��ɫ
    BLUE:
      FRichEditLog.SelAttributes.Color := clWebDeepskyBlue;        //��ɫ
    MAGENTA:
      FRichEditLog.SelAttributes.Color := clWebMagenta;            //Ʒ��
    CYAN:
      FRichEditLog.SelAttributes.Color := clWebCyan;               //��ɫ
    GREY:
      FRichEditLog.SelAttributes.Color := clWebLightgrey;          //��ɫ
    YELLOW:
      FRichEditLog.SelAttributes.Color := clYellow;                //��ɫ
    LRED:
      FRichEditLog.SelAttributes.Color := clWebDarkRed;            //��ɫ����
    LGREEN:
      FRichEditLog.SelAttributes.Color := clWebLightGreen;         //��ɫ����
    LBLUE:
      FRichEditLog.SelAttributes.Color := clWebLightBlue;          //��ɫ����
    LMAGENTA:
      FRichEditLog.SelAttributes.Color := clWebDarkMagenta;        //Ʒ�����
    LCYAN:
      FRichEditLog.SelAttributes.Color := clWebLightCyan;          //��ɫ����
    WHITE:
      FRichEditLog.SelAttributes.Color := clWhite;                 //��ɫ
  end;
end;

procedure TLog.setSource(Source: TRichEdit);
begin
  FRichEditLog := Source;
end;

procedure TLog.ShowMemoLogMsg;
var
  i: Integer;
  MemoLogMsg: TMemoLogMsg;
begin
  if (GetTickCount() - FdwShowMemoLogTick) < 200 then
    Exit;

  FdwShowMemoLogTick := GetTickCount();

  //�б�������0
  if FMemoLogMsgList.LockList.Count <= 0 then
    Exit;

  with FMemoLogMsgList.LockList do
  begin
    try
      for i := 0 to Count - 1 do
      begin
        MemoLogMsg := Items[i];
        if MemoLogMsg.sMag <> '' then
          outDefault(MemoLogMsg.sMag, MemoLogMsg.Color);
      end;
      Clear;
    finally
      FMemoLogMsgList.UnlockList;
    end;
  end;
end;

procedure TLog.TimerEvent(Sender: TObject);
begin
  try
    //ʹ���ڲ���ʱ��ִ��
    ShowMemoLogMsg();
  except

  end;
end;

initialization
begin
  sLog := TLog.Create;
end;

finalization
begin
  sLog.Free;
end;



end.

