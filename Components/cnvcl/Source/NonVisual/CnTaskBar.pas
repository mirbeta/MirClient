{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTaskBar;
{* |<PRE>
================================================================================
* 软件名称：不可视组件包
* 单元名称：任务栏控制组件实现单元
* 单元作者：不得闲
* 移    植：Childe Ng
* 备    注：
* 开发平台：PWinXpPro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnTaskBar.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* 修改记录：2008.08.28 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, Messages, SysUtils, Forms, ShellAPI, CommCtrl, Graphics;

type               
  TCnSysToolBarBtn = class(TPersistent)
  {* 任务栏按钮描述}
  private
    FBtnInfo: TTBButton;
    FBtnIndex: Integer;
    FBtnCaption: string;
    SysHide: Boolean;//是否为系统隐藏图标
    EventHandle: THandle; //事件处理句柄
    FPicture: TBitmap; //图标
    FBtnRect: TRect;  //区域
    FVisible: Boolean;
    FEnabled: Boolean;
    IsTrayBtn: Boolean;
    procedure SetVisible(const Value: Boolean);
    Constructor Create;
    procedure SetEnabled(const Value: Boolean);
  public
    Destructor Destroy;override;
    property BtnInfo: TTBButton read FBtnInfo;
    property BtnIndex: Integer read FBtnIndex;
    property BtnCaption: string read FBtnCaption;
    property BtnRect: TRect read FBtnRect;
    procedure AssignBtnInfo(Info: TTBButton);
    property IsSysHide: Boolean read SysHide;
    property Picture: TBitmap read FPicture;
    property Handle: THandle read EventHandle;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    procedure Click;
    procedure DbClick;
    procedure RClick;
  end;

  TCnTaskBar = Class(TComponent) 
  {* 任务栏操作组件}
  private
    FTrayBtnList, FTaskBtnList: TstringList;
    HigherThenXp: Boolean; //是否为xp以上的系统版本
    FTrayBarHandle: THandle;
    FTaskBarHandle: THandle;
    FStartBtnHandle: THandle;
    FQuitLauchHandle: THandle;
    FReBarHandle: THandle;
    FProgramToolBarHandle: THandle;
    FImeRecHandle: THandle;
    FProgramContrainerHandle: THandle; //任务栏容器
    FHideTrayBtnHandle: THandle;
    FTrayNotifyHandle: THandle;
    FClockHandle: THandle;
    FShowHideBtn: Boolean;
    FVisible: Boolean;
    FQuickBarVisible: Boolean;
    FTaskToolBarVisible: Boolean;
    FTaskBarVisible: Boolean;
    FRegBarVisible: Boolean;
    FStartBtnVisible: Boolean;
    FImeBarVisible: Boolean;
    FTrayBarVisible: Boolean;
    FStartBtnEnabled: Boolean;
    function GetTrayBtnCount: Integer;
    function IsSysBtnHide(BtnState: Dword): Boolean;
    function GetTrayBtns(Index: Integer): TCnSysToolBarBtn;
    function GetTaskBtnCount: Integer;
    function GetTaskBtns(Index: Integer): TCnSysToolBarBtn;
    function GetStartBtnCaption: string;
    procedure GetIconList;
    procedure GetTaskList;
    procedure SetShowHideBtn(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetQuickBarVisible(const Value: Boolean);
    procedure SetTaskToolBarVisible(const Value: Boolean);
    procedure SetTaskBarVisible(const Value: Boolean);
    procedure SetReBaVisible(const Value: Boolean);
    procedure SetStartBtnVisible(const Value: Boolean);
    procedure SetImeBarVisible(const Value: Boolean);
    procedure SetTrayBarVisible(const Value: Boolean);
    procedure SetStartBtnCaption(const Value: string);
    procedure SetStartBtnEnabled(const Value: Boolean);
  protected
    procedure StartBtnWndProc(var Message: TMessage);
  public
    Constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetTimeDlg;       //设置时间对话框
    procedure HideTrayBtnClick; //显示隐藏按扭单击
    procedure ImeRectBtnClick;//输入法按扭单击
    procedure ClearTrayBtnList; //清除托盘区列表
    procedure ClearTaskBtnList;
    procedure ShowTime;
    procedure StartBtnClick;
    procedure HideOn;
    procedure ShowOn;
    property TrayBarHandle: THandle read FTrayBarHandle;//托盘区句柄
    property TaskBarHandle: THandle read FTaskBarHandle;//任务栏句柄
    property StartBtnHandle: THandle read FStartBtnHandle;//开始按扭句柄
    property QuitLauchHandle: THandle read FQuitLauchHandle;//快速启动栏句柄
    property ImeRecHandle: THandle read FImeRecHandle;//输入法选择区域句柄
    property ProgramToolBarHandle: THandle read FProgramToolBarHandle;//程序最小化按扭容器
    property HideTrayBtnHandle: THandle read FHideTrayBtnHandle;//显示隐藏图标的按扭
    property ClockHandle: THandle read FClockHandle;//时钟显示区域
    property TrayBtnList: TstringList read FTrayBtnList;
    property TaskBtnList: TstringList read FTaskBtnList;
    property TrayBtnCount: Integer read GetTrayBtnCount;//托盘图标的个数
    property TaskBtnCount: Integer read GetTaskBtnCount;//任务栏应用程序按扭个数
    property TrayBtns[index: Integer]: TCnSysToolBarBtn read GetTrayBtns; //托盘按扭
    Property TaskBtns[index: Integer]: TCnSysToolBarBtn read GetTaskBtns; //任务栏按扭
  published
    property TrayBarVisible: Boolean read FTrayBarVisible write SetTrayBarVisible; //托盘区域隐藏
    property ImeBarVisible: Boolean read FImeBarVisible write SetImeBarVisible; //语言区域隐藏
    property ReBarVisible: Boolean read FRegBarVisible write SetReBaVisible;//任务栏按扭隐藏
    property TaskToolBarVisible: Boolean read FTaskToolBarVisible write SetTaskToolBarVisible;//任务栏应用程序区域隐藏
    property TaskBarVisible: Boolean read FTaskBarVisible write SetTaskBarVisible;//和上面一样
    property QuickBarVisible: Boolean read FQuickBarVisible write SetQuickBarVisible;//快速启动栏
    property Visible: Boolean read FVisible write SetVisible; //是否隐藏任务栏
    property ShowHideBtn: Boolean read FShowHideBtn write SetShowHideBtn;//是否显示系统隐藏的托盘按扭
    property StartBtnVisible: Boolean read FStartBtnVisible write SetStartBtnVisible;//开始按扭隐藏
    property StartBtnCaption: string read GetStartBtnCaption write SetStartBtnCaption; //开始按扭
    property StartBtnEnabled: Boolean read FStartBtnEnabled write SetStartBtnEnabled;//开始按扭
  end;

implementation

var
  hWndTip: DWORD;
  ToolInfo: TToolInfo;
  
//得到BitNum的二进制位上的第bitPos位上的数字是为1还是为0
function GetBitNum(bitPos: ShortInt; bitNum: Integer): ShortInt;
begin
  Result := BitNum shr (BitPos - 1) and 1;  //位数从1开始
end;

procedure AddTipTool(hWnd: DWORD; IconType: Integer;
  Title,  Text: PChar; BackColor,  FontColor: TColor);
const
  TTS_BALLOON = $0040;
  TTM_SETTITLE = WM_USER + 32;
begin
  hWndTip:= CreateWindow(TOOLTIPS_CLASS, nil,
                        WS_POPUP or TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 
                        0, 0, 0, 0, hWnd, 0, HInstance, nil);
  if hWndTip > 0 then
  begin
    ToolInfo.cbSize:= SizeOf(ToolInfo);
    ToolInfo.uFlags:= TTF_IDISHWND or TTF_SUBCLASS or TTF_TRANSPARENT;
    ToolInfo.lpszText:= Text;
    ToolInfo.uId:= hWnd;
    SendMessage(hWndTip, TTM_SETTIPBKCOLOR, BackColor, 0);
    SendMessage(hWndTip, TTM_SETTIPTEXTCOLOR, FontColor, 0);
    SendMessage(hWndTip, TTM_ADDTOOL, 0, Integer(@ToolInfo));
    SendMessage(hWndTip, TTM_SETTITLE, IconType, Integer(Title));
  end;
  InitCommonControls();
end;

{ TCnTaskBar }

procedure TCnTaskBar.ClearTaskBtnList;
begin
  while FTaskBtnList.Count > 0 do
  begin
    FTaskBtnList.Objects[FTaskBtnList.Count - 1].Free;
    FTaskBtnList.Delete(FTaskBtnList.Count - 1);
  end;
end;

procedure TCnTaskBar.ClearTrayBtnList;
begin
  while FTrayBtnList.Count > 0 do
  begin
    FTrayBtnList.Objects[FTrayBtnList.Count - 1].Free;
    FTrayBtnList.Delete(FTrayBtnList.Count - 1);
  end;
end;

constructor TCnTaskBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrayBtnList := TstringList.Create;
  FTaskBtnList := TstringList.Create;
  HigherThenXp := (Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion > 0));
  FTaskBarHandle := FindWindow('Shell_TrayWnd', nil);
  FStartBtnHandle := FindWindowEx(FTaskBarHandle, 0, 'Button', nil);
  //if (OS.dwMajorVersion = 4) and (OS.dwMinorVersion = 10)  then //98系统
  FReBarHandle := FindWindowEx(FTaskBarHandle, 0, 'ReBarWindow32', nil);
  FQuitLauchHandle := FReBarHandle;
  FProgramContrainerHandle := FindWindowEx(FQuitLauchHandle, 0, 'MSTaskSwWClass', nil);
  FImeRecHandle := FindWindowEx(FQuitLauchHandle, 0, 'CiceroUIWndFrame', nil);

  if HigherThenXp then
    FProgramToolBarHandle := FindWindowEx(FProgramContrainerHandle, 0, 'ToolbarWindow32', nil)
  else FProgramToolBarHandle := FProgramContrainerHandle;
  FTrayBarHandle := FindWindowEx(FTaskBarHandle, 0, 'TrayNotifyWnd', nil);
  FTrayNotifyHandle := FTrayBarHandle;
  FClockHandle := FindWindowEx(FTrayBarHandle, 0, 'TrayClockWClass', nil);
  FHideTrayBtnHandle := FindWindowEx(FTrayBarHandle, 0, 'Button', nil);
  if HigherThenXp then
    FTrayBarHandle := FindWindowEx(FTrayBarHandle, 0, 'SysPager', nil);
  if (Win32MajorVersion = 5) and (Win32MinorVersion >= 0) then
    FTrayBarHandle := FindWindowEx(FTrayBarHandle, 0, 'ToolbarWindow32', nil);

  FQuitLauchHandle := FindWindowEx(FQuitLauchHandle, 0, 'ToolbarWindow32', nil);//快速启动栏
  //SetWindowLong(FStartBtnHandle,  GWL_WNDPROC,  Longint(MakeObjectInstance(StartBtnWndProc)));
  GetIconList;
  GetTaskList;
  Visible := True;
  ReBarVisible := True;
  TaskBarVisible := True;
  QuickBarVisible := True;
  TaskToolBarVisible := True;
  StartBtnVisible := True;
  TrayBarVisible := True;
  ImeBarVisible := True;
end;

destructor TCnTaskBar.Destroy;
begin
  ClearTrayBtnList;
  FTrayBtnList.Free;
  ClearTaskBtnList;
  FTaskBtnList.Free;
  inherited;
end;

procedure TCnTaskBar.GetIconList;
var
  ThreadID: THandle;
  ThreadHandle: THandle; //线程句柄
  Buff: pchar;
  i, BtnCount: Integer;
  R: Cardinal;
  BtnInfo: TTBButton;
  SysHide: Boolean;
  SysToolBtn: TCnSysToolBarBtn;
  S: array[0..512] of char;
  BtnRect: TRect;
begin
  GetWindowThreadProcessId(FTrayBarHandle,  @ThreadID);//获取托盘窗口的线程 ID
  ThreadHandle := OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE,  False,  ThreadID);//得到线程句柄
  Buff := VirtualAllocEx(ThreadHandle, nil, 4096, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);//指定进程的虚拟空间保留或提交内存区域，除非指定MEM_RESET参数，否则将该内存区域置0
  BtnCount := SendMessage(FTrayBarHandle, TB_BUTTONCOUNT, 0,  0);//得到托盘按扭个数
  //SendMessage(FTrayBarHandle, TB_GETIMAGELIST, 0, 0);
  //SendMessage(FTrayBarHandle, TB_GETBITMAPFLAGS, 0, 0);
  try
    for i := 0 to BtnCount - 1 do
    begin
      WriteProcessMemory(ThreadHandle, Buff, @BtnInfo, SizeOf(BtnInfo), R);
      SendMessage(FTrayBarHandle, TB_GETBUTTON, i, Integer(Buff));
      ReadProcessMemory(ThreadHandle, Buff, @BtnInfo, SizeOf(BtnInfo), R);
      SysHide := IsSysBtnHide(BtnInfo.fsState);
      if SysHide and (not FShowHideBtn) then
        Continue;

      SysToolBtn := TCnSysToolBarBtn.Create;
      SysToolBtn.SysHide := SysHide;
      SysToolBtn.FVisible := not SysHide;
      SysToolBtn.AssignBtnInfo(BtnInfo);
      SysToolBtn.IsTrayBtn := True;
      //SysToolBtn.FPicture.Canvas
      SysToolBtn.FBtnIndex := BtnInfo.idCommand;
      SendMessage(FTrayBarHandle, TB_GETBUTTONTEXT, SysToolBtn.FBtnInfo.idCommand, Integer(Integer(@Buff[0]) + SizeOf(@SysToolBtn.FBtnInfo)));
      ReadProcessMemory(ThreadHandle,  Pointer(Integer(@Buff[0]) + SizeOf(@SysToolBtn.FBtnInfo)), @S[0], SizeOf(S),  R);
      //if SysToolBtn.FBtnInfo.fsState = 12 then
      SysToolBtn.FBtnCaption := string(s);
      SysToolBtn.EventHandle := FTrayBarHandle;
      if not SysHide then
      begin
        SendMessage(FTrayBarHandle, TB_GETRECT, BtnInfo.idCommand, Integer(Integer(@Buff[0]) + SizeOf(BtnInfo)));
        ReadProcessMemory(ThreadHandle, Pointer(Integer(@Buff[0]) + SizeOf(BtnInfo)),  @BtnRect, SizeOf(BtnRect), R);//得到Rect信息
        SysToolBtn.FBtnRect :=  BtnRect;

        SysToolBtn.FPicture.Width :=  BtnRect.Right - BtnRect.Left;
        SysToolBtn.FPicture.Height :=  BtnRect.Bottom - BtnRect.Top;

        BitBlt(SysToolBtn.FPicture.Canvas.Handle, 0, 0, SysToolBtn.FPicture.Width, SysToolBtn.FPicture.Height, 
               GetDc(FTrayBarHandle), BtnRect.Left, BtnRect.Top, SRCCOPY); //抓图
      end;
      FTrayBtnList.AddObject(SysToolBtn.FBtnCaption, SysToolBtn);
      {if BtnInfo.fsState <> TBSTATE_HIDDEN then //如果是隐藏的，则不显示出来
      begin
         //FTrayBtnList.Add(s)
      end;}
    end;
  finally
    VirtualFreeEx(ThreadHandle, Buff, 0, MEM_RELEASE);
    CloseHandle(ThreadHandle);
  end;
end;

function TCnTaskBar.GetTaskBtnCount: Integer;
begin
  Result := FTaskBtnList.Count;
end;

function TCnTaskBar.GetTaskBtns(Index: Integer): TCnSysToolBarBtn;
begin                                       
   if (Index > -1 ) and (Index < FTaskBtnList.Count) then
     Result :=  TCnSysToolBarBtn(FTaskBtnList.Objects[Index])
   else Result :=  nil;
end;

procedure TCnTaskBar.GetTaskList;
var
  i, BtnCount: Integer;
  ThreadId: LongInt;
  ThreadHandle: THandle;
  vBuffer: array[0..255] of Char;
  SysToolBtn: TCnSysToolBarBtn;
  BtnInfo: TTBButton;
  Buff: pointer;
  BtnRect: TRect;
  WriteNum: Cardinal;
  SysHide: Boolean;
begin
  BtnCount :=  SendMessage(FProgramToolBarHandle, TB_BUTTONCOUNT, 0, 0);
  GetWindowThreadProcessId(FProgramToolBarHandle, @ThreadId);
  ThreadHandle :=  OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_READ or
                              PROCESS_VM_WRITE, False, ThreadId);
  Buff :=  VirtualAllocEx(ThreadHandle, nil, 4096, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
  try
    for i := 0 to BtnCount - 1 do
    begin
      WriteProcessMemory(ThreadHandle, Buff, @BtnInfo, Sizeof(BtnInfo), WriteNum);
      SendMessage(FProgramToolBarHandle, TB_GETBUTTON,  i, Integer(Buff));
      ReadProcessMemory(ThreadHandle, Buff, @BtnInfo, SizeOf(BtnInfo), WriteNum);

      SendMessage(FProgramToolBarHandle, TB_GETRECT, BtnInfo.idCommand, Integer(Integer(Buff) + SizeOf(BtnInfo)));
      ReadProcessMemory(ThreadHandle, Pointer(Integer(Buff) + SizeOf(BtnInfo)),  @BtnRect, SizeOf(BtnRect), WriteNum);//得到Rect信息
      SysHide := (BtnRect.Right - BtnRect.Left = 0) and (BtnRect.Bottom - BtnRect.Top  = 0);
      SysHide := IsSysBtnHide(BtnInfo.fsState) or SysHide;
      if SysHide and (not FShowHideBtn) then
        Continue;

      SysToolBtn := TCnSysToolBarBtn.Create;
      SysToolBtn.SysHide := SysHide;
      SysToolBtn.FVisible := not SysHide;
      SysToolBtn.AssignBtnInfo(BtnInfo);
      SysToolBtn.IsTrayBtn := false;
      //SysToolBtn.FPicture.Canvas
      SysToolBtn.FBtnIndex := BtnInfo.idCommand;
      SendMessage(FProgramToolBarHandle, TB_GETBUTTONTEXT, SysToolBtn.FBtnInfo.idCommand, Integer(Integer(Buff) + SizeOf(@SysToolBtn.FBtnInfo)));
      ReadProcessMemory(ThreadHandle, Pointer(Integer(Buff) + SizeOf(@SysToolBtn.FBtnInfo)), @VBuffer, SizeOf(VBuffer),  WriteNum);
      SysToolBtn.FBtnCaption := string(VBuffer);

      SysToolBtn.EventHandle :=  FProgramToolBarHandle;
      SysToolBtn.FBtnRect :=  BtnRect;
      FTaskBtnList.AddObject(SysToolBtn.FBtnCaption, SysToolBtn);
    end;
  finally
    VirtualFreeEx(ThreadHandle,  Buff, 0,  MEM_RELEASE);
    CloseHandle(ThreadHandle);
  end;
end;

function TCnTaskBar.GetTrayBtnCount: Integer;
begin
   Result :=  FTrayBtnList.Count;
end;

function TCnTaskBar.GetTrayBtns(Index: Integer): TCnSysToolBarBtn;
begin
   if (Index > -1 ) and (Index < FTrayBtnList.Count) then
     Result :=  TCnSysToolBarBtn(FTrayBtnList.Objects[Index])
   else Result :=  nil;
end;                                      

procedure TCnTaskBar.HideTrayBtnClick;
begin
  PostMessage(FHideTrayBtnHandle, WM_LButtonDown, 0, 0);
  PostMessage(FHideTrayBtnHandle, WM_LButtonUp, 0, 0);
end;

procedure TCnTaskBar.ImeRectBtnClick;
begin
  PostMessage(FImeRecHandle, WM_LButtonDown, 0, MakeLParam(4, 5));
  PostMessage(FImeRecHandle, WM_LButtonUp, 0, MakeLParam(4, 5));
end;

function TCnTaskBar.IsSysBtnHide(BtnState: Dword): Boolean;
begin
  Result :=  GetBitNum(4, BtnState) = 1;
end;

procedure TCnTaskBar.SetReBaVisible(const Value: Boolean);
begin
  if (FRegBarVisible <> Value) and Visible then
  begin
    FRegBarVisible :=  Value;
    if Value then
      ShowWindow(FReBarHandle, SW_Normal)
    else
      ShowWindow(FReBarHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetQuickBarVisible(const Value: Boolean);
begin
  if (FQuickBarVisible <> Value) and TaskBarVisible then
  begin
    FQuickBarVisible :=  Value;
    if Value then
      ShowWindow(FQuitLauchHandle, SW_Normal)
    else
      ShowWindow(FQuitLauchHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetShowHideBtn(const Value: Boolean);
begin
  if FShowHideBtn <> Value then
  begin
    FShowHideBtn :=  Value;
    ClearTrayBtnList;
    GetIconList;
    ClearTaskBtnList;
    GetTaskList;
  end;
end;

procedure TCnTaskBar.SetTaskBarVisible(const Value: Boolean);
begin
  if (FTaskBarVisible <> Value) and FRegBarVisible then
  begin
    FTaskBarVisible :=  Value;
    if Value then
      ShowWindow(FProgramContrainerHandle, SW_Normal)
    else
      ShowWindow(FProgramContrainerHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetTaskToolBarVisible(const Value: Boolean);
begin
  if (FTaskToolBarVisible <> Value) and (FTaskBarVisible) then
  begin
    FTaskToolBarVisible :=  Value;
    if Value then
      ShowWindow(FProgramToolBarHandle, SW_Normal)
    else
      ShowWindow(FProgramToolBarHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetTimeDlg;
begin
  winexec('rundll32.exe   shell32.dll, Control_RunDLL   timedate.cpl', 9);
  //SendMessage(FClockHandle,  WM_LBUTTONDBLCLK, 0, MakeLong(2, 2));
  //SendMessage(FClockHandle, WM_LBUTTONUP, 0, MakeLong(2, 2));
end;

procedure TCnTaskBar.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible :=  Value;
    if FVisible then
      ShowWindow(FTaskBarHandle, SW_NORMAL)
    else
      ShowWindow(FTaskBarHandle, SW_HIDE);
  end;
end;

procedure TCnTaskBar.ShowTime;
begin
  AddTipTool(FProgramToolBarHandle, 1, PChar('时间显示'),
             PChar(DateToStr(now)), $00FFBFBF, $00A60053);//添加气泡提示
end;

procedure TCnTaskBar.StartBtnClick;
begin
  SendMessage(self.FStartBtnHandle, WM_LBUTTONDOWN, 0, 0);
  SendMessage(self.FStartBtnHandle, WM_LBUTTONUP, 0, 0);
end;

procedure TCnTaskBar.StartBtnWndProc(var Message: TMessage);
begin
 { if Message.Msg = WM_LButtonup then
    ShowMessage('sdf');}
end;

procedure TCnTaskBar.SetStartBtnVisible(const Value: Boolean);
begin
  if (FStartBtnVisible <> Value) and Visible then
  begin
    FStartBtnVisible := Value;
    if Value then
      ShowWindow(FStartBtnHandle, SW_Normal)
    else
      ShowWindow(FStartBtnHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetImeBarVisible(const Value: Boolean);
begin
  if (FImeBarVisible <> Value) and FRegBarVisible then
  begin
    FImeBarVisible :=  Value;
    if Value then
      ShowWindow(FImeRecHandle, SW_Normal)
    else
      ShowWindow(FImeRecHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.SetTrayBarVisible(const Value: Boolean);
begin
  if (FTrayBarVisible <> Value) and FVisible then
  begin
    FTrayBarVisible := Value;
    if Value then
      ShowWindow(FTrayNotifyHandle, SW_Normal)
    else
      ShowWindow(FTrayNotifyHandle, SW_Hide);
  end;
end;

procedure TCnTaskBar.HideOn;
begin
  ReBarVisible := False;
  TrayBarVisible := False;
  StartBtnVisible := False;
end;

procedure TCnTaskBar.ShowOn;
begin
  ReBarVisible := True;
  TaskBarVisible := True;
  QuickBarVisible := True;
  TaskToolBarVisible := True;
  StartBtnVisible := True;
  TrayBarVisible := True;
  ImeBarVisible := True;
end;

{ TCnSysToolBarBtn }

procedure TCnSysToolBarBtn.AssignBtnInfo(Info: TTBButton);
begin
  FBtnInfo.iBitmap := Info.iBitmap;
  FBtnInfo.idCommand := Info.idCommand;
  FBtnInfo.fsState := Info.fsState;
  FBtnInfo.fsStyle := Info.fsStyle;
  FBtnInfo.bReserved := Info.bReserved;
  FBtnInfo.dwData := Info.dwData;
  FBtnInfo.istring := Info.istring;
end;

procedure TCnSysToolBarBtn.Click;
begin
  SendMessage(EventHandle, WM_LBUTTONDOWN, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
  SendMessage(EventHandle, WM_LBUTTONUP, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
end;

constructor TCnSysToolBarBtn.Create;
begin
  Inherited Create;
  FillChar(FBtnInfo, SizeOf(FBtnInfo), 0);
  FPicture :=  TBitMap.Create;
end;

procedure TCnSysToolBarBtn.DbClick;
begin
   SendMessage(EventHandle, WM_LBUTTONDBLCLK, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
   SendMessage(EventHandle, WM_LBUTTONUP, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
end;

destructor TCnSysToolBarBtn.Destroy;
begin
  FPicture.Free;
  if (not isSysHide) and (not FVisible) then
    Visible :=  True
  else if IsSysHide and FVisible then
    Visible :=  False;
  ZeroMemory(Pointer(@FBtnInfo), Sizeof(FBtnInfo));
  inherited;
end;

procedure TCnSysToolBarBtn.RClick;
begin
  SendMessage(EventHandle, WM_RBUTTONDOWN, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
  SendMessage(EventHandle, WM_RBUTTONUP, 0, MakeLong(FBtnRect.Left + 2, FBtnRect.Top + 2));
end;

procedure TCnSysToolBarBtn.SetEnabled(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible :=  Value;
    EnableWindow(EventHandle, Value);
  end;
end;

procedure TCnSysToolBarBtn.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible :=  Value;
    if FVisible then
      SendMessage(EventHandle, TB_HIDEBUTTON, BtnInfo.idCommand, 0)
    else
      SendMessage(EventHandle, TB_HIDEBUTTON, BtnInfo.idCommand, 1);
  end;
end;

procedure TCnTaskBar.SetStartBtnCaption(const Value: string);
begin
  SendMessage(FStartBtnHandle, WM_SETTEXT, 0, Integer(PChar(Value)));
end;

function TCnTaskBar.GetStartBtnCaption: string;
var
  buff: array[0..255] of char;
begin
  SendMessage(FStartBtnHandle, WM_GETTEXT, 256, Integer(@Buff));
  Result :=  buff;
end;

procedure TCnTaskBar.SetStartBtnEnabled(const Value: Boolean);
begin
  if  FStartBtnEnabled <> Value then
  begin
    FStartBtnEnabled :=  Value;
    EnableWindow(FStartBtnHandle, Value);
  end;
end;

end.





