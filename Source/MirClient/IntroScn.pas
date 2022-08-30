unit IntroScn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Controls, Forms, Dialogs, uGameEngine, uCommon,
  ExtCtrls, PXL.Canvas, PXL.Textures, PXL.Devices, PXL.Types, AsphyreTextureFonts, FState, Grobal2, cliUtil, ClFunc, SoundUtil,
  HUtil32, DWinCtl;

const
  SELECTEDFRAME = 16;
  FREEZEFRAME = 13;
  EFFECTFRAME = 14;

type
  TLoginState = (lsLogin, lsNewid, lsNewidRetry, lsChgpw, lsCloseAll);
  TSceneType = (stIntro, stLogin, stSelectCountry, stSelectChr, stNewChr, stLoading, stLoginNotice, stPlayGame);
  TSelChar = record
    Valid: Boolean;
    UserChr: TUserCharacterInfo;
    Selected: Boolean;
    FreezeState: Boolean;
    Unfreezing: Boolean;
    Freezing: Boolean;
    AniIndex: Integer;
    DarkLevel: Integer;
    EffIndex: Integer;
    StartTime: LongWord;
    moretime: LongWord;
    startefftime: LongWord;
  end;

  TScene = class
  private
  public
    scenetype: TSceneType;
    constructor Create(scenetype: TSceneType);
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure OpenScene; virtual;
    procedure CloseScene; virtual;
    procedure OpeningScene; virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure PlayScene(MSurface: TCustomCanvas); virtual;
  end;

  TIntroScene = class(TScene)
    m_boOnClick: Boolean;
    m_dwStartTime: LongWord;
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenScene; override;
    procedure CloseScene; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PlayScene(MSurface: TCustomCanvas); override;
  end;

  TLoginScene = class(TScene)
  private
    //m_EdPasswd: TEdit;
    m_EdNewId: TEdit;
    m_EdNewPasswd: TEdit;
    m_EdConfirm: TEdit;
    m_EdYourName: TEdit;
    m_EdSSNo: TEdit;
    m_EdBirthDay: TEdit;
    m_EdQuiz1: TEdit;
    m_EdAnswer1: TEdit;
    m_EdQuiz2: TEdit;
    m_EdAnswer2: TEdit;
    m_EdPhone: TEdit;
    m_EdMobPhone: TEdit;
    m_EdEMail: TEdit;
    m_EdChgId: TEdit;
    m_EdChgCurrentpw: TEdit;
    m_EdChgNewPw: TEdit;
    m_EdChgRepeat: TEdit;
    m_nCurFrame: Integer;
    m_nMaxFrame: Integer;
    m_dwStartTime: LongWord;
    m_boNowOpening: Boolean;
    m_boOpenFirst: Boolean;
    m_NewIdRetryUE: TUserEntryA;

    function CheckUserEntrys: Boolean;
    function NewIdCheckNewId: Boolean;
    function NewIdCheckSSno: Boolean;
    function NewIdCheckBirthDay: Boolean;
  public
    m_sLoginId: string;
    m_sLoginPasswd: string;
    //m_boUpdateAccountMode: Boolean;
    m_EditIDHandle: THandle;
    m_EditIDPointer: Pointer;
    m_EditPassHandle: THandle;
    m_EditPassPointer: Pointer;
    constructor Create;
    destructor Destroy; override;
    procedure OpenScene; override;
    procedure CloseScene; override;

    procedure EdLoginIdKeyPress(Sender: TObject; var Key: Char);
    procedure EdLoginPasswdKeyPress(Sender: TObject; var Key: Char);
    procedure EdNewIdKeyPress(Sender: TObject; var Key: Char);
    procedure EdNewOnEnter(Sender: TObject);

    procedure PlayScene(MSurface: TCustomCanvas); override;
    procedure ChangeLoginState(State: TLoginState);
    procedure NewClick;
    procedure NewIdRetry(boupdate: Boolean);
    procedure UpdateAccountInfos(ue: TUserEntryA);
    procedure OkClick;
    procedure ChgPwClick;
    procedure NewAccountOk;
    procedure NewAccountClose;
    procedure ChgpwOk;
    procedure ChgpwCancel;
    procedure HideLoginBox;
    procedure OpenLoginDoor;
    procedure PassWdFail;
    procedure EditIDWndProc(var Message: TMessage);
    procedure EditPassWndProc(var Message: TMessage);
  end;

  TSelectChrScene = class(TScene)
  private
    SoundTimer: TTimer;
    CreateChrMode: Boolean;
    //EdChrName: TEdit;
    procedure SoundOnTimer(Sender: TObject);
    procedure MakeNewChar(Index: Integer);
    //procedure EdChrnameKeyPress(Sender: TObject; var Key: Char);
  public
    NewIndex: Integer;
    ChrArr: array[0..1] of TSelChar;
    constructor Create;
    destructor Destroy; override;
    procedure OpenScene; override;
    procedure CloseScene; override;
    procedure PlayScene(MSurface: TCustomCanvas); override;
    procedure SelChrSelect1Click;
    procedure SelChrSelect2Click;
    procedure SelChrStartClick;
    procedure SelChrNewChrClick;
    procedure SelChrEraseChrClick;
    procedure SelChrCreditsClick;
    procedure SelChrExitClick;
    procedure SelChrNewClose;
    procedure SelChrNewJob(job: Integer);
    procedure SelChrNewm_btSex(sex: Integer);
    procedure SelChrNewPrevHair;
    procedure SelChrNewNextHair;
    procedure SelChrNewOk;
    procedure ClearChrs;
    procedure AddChr(uname: string; job, hair, Level, sex: Integer);
    procedure SelectChr(Index: Integer);
  end;

  TLoginNotice = class(TScene)
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  ClMain, MShare, EDcode;

constructor TScene.Create(scenetype: TSceneType);
begin
  scenetype := scenetype;
end;

procedure TScene.Initialize;
begin
end;

procedure TScene.Finalize;
begin
end;

procedure TScene.OpenScene;
begin
  ;
end;

procedure TScene.CloseScene;
begin
  ;
end;

procedure TScene.OpeningScene;
begin
end;

procedure TScene.KeyPress(var Key: Char);
begin
end;

procedure TScene.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TScene.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TScene.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TScene.PlayScene(MSurface: TCustomCanvas);
begin
  ;
end;

{------------------- TIntroScene ----------------------}

constructor TIntroScene.Create;
begin
  inherited Create(stIntro);
end;

destructor TIntroScene.Destroy;
begin
  inherited Destroy;
end;

procedure TIntroScene.OpenScene;
begin
  m_boOnClick := False;
  m_dwStartTime := GetTickCount + 3 * 1000;
end;

procedure TIntroScene.CloseScene;
begin

end;

procedure TIntroScene.KeyPress(var Key: Char);
begin
  m_boOnClick := True;
end;

procedure TIntroScene.KeyDown(var Key: Word; Shift: TShiftState);
begin
  m_boOnClick := True;
end;

procedure TIntroScene.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  m_boOnClick := True;
end;

procedure TIntroScene.PlayScene(MSurface: TCustomCanvas);
resourcestring
//  COPYRIGHTNAME = 'LEGEND';
  COPYRIGHTNAME             = 'BLUE';
var
  n: Integer;
  s: string;
  d, dd: TCustomLockableTexture;
begin
//  MSurface.Fill(clWhite);
  //d := g_WMainUibImages.Images[g_sLogoUIB];
  //if d <> nil then
  //  MSurface.Draw((SCREENWIDTH - d.Width) div 2, (SCREENHEIGHT - d.Height) div 2, d.ClientRect, d, True);

  MSurface.FillRectAlpha(IntRectBDS(0,0,SCREENWIDTH,SCREENHEIGHT),clWhite,255);
  d := FontManager.GetFont('黑体', 145, [fsBold]).TextOut(COPYRIGHTNAME);
  if d <> nil then
  begin
      MSurface.DrawBoldText((SCREENWIDTH - d.WIDTH) div 2,
        (SCREENHEIGHT - d.Height) div 2, d, GetRGB(249), GetRGB(249))
  end;


//  n := MSurface.Canvas.Font.Size;
//  MSurface.Canvas.Font.Size := 145;
//  s := MSurface.Canvas.Font.Name;
//  MSurface.Canvas.Font.Name := '黑体';
//  MSurface.Canvas.Font.Style := [fsBold];
//  try
//    BoldTextOut(MSurface,
//      (SCREENWIDTH - MSurface.Canvas.TextWidth(COPYRIGHTNAME, True, 145)) div 2,
//      (SCREENHEIGHT - MSurface.Canvas.TextHeight(COPYRIGHTNAME, True, 145)) div 2,
//      GetRGB(249),
//      GetRGB(249),
//      COPYRIGHTNAME);
//  finally
//    MSurface.Canvas.Font.Size := n;
//    MSurface.Canvas.Font.Name := s;
//    MSurface.Canvas.Font.Style := [];
//  end;

  if GetTickCount > m_dwStartTime then begin
    m_boOnClick := True;
    DScreen.ChangeScene(stLogin);
    if not g_boDoFadeOut and not g_boDoFadeIn then begin
      //g_boDoFadeOut := True;
      g_boDoFadeIn := True;
      g_nFadeIndex := 0;
    end;
  end;
end;

{--------------------- Login ----------------------}

procedure TLoginScene.EditIDWndProc(var Message: TMessage);
begin
  //HideCaret(m_EditIDHandle);
  case Message.Msg of
    EM_SETSEL,
      WM_KEYFIRST,
      WM_LBUTTONDBLCLK,
      WM_RBUTTONDOWN,
      WM_RBUTTONUP,
      WM_COPY,
      WM_MOUSEMOVE,
      WM_SETCURSOR,
      WM_GETTEXT,
      WM_MOUSELEAVE: Exit;
  end;
  Message.Result := CallWindowProc(m_EditIDPointer, m_EditIDHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TLoginScene.EditPassWndProc(var Message: TMessage);
begin
  //HideCaret(m_EditPassHandle);
  case Message.Msg of
    EM_SETSEL, WM_KEYFIRST, WM_LBUTTONDBLCLK,
      WM_RBUTTONDOWN, WM_RBUTTONUP,
      WM_COPY, WM_MOUSEMOVE, WM_SETCURSOR,
      WM_GETTEXT, WM_MOUSELEAVE: Exit;
  end;
  Message.Result := CallWindowProc(m_EditPassPointer, m_EditPassHandle, Message.Msg, Message.WParam, Message.LParam);
end;

constructor TLoginScene.Create;
var
  p: Pointer;
  nX, nY: Integer;
begin
  inherited Create(stLogin);
  m_boOpenFirst := False;

  nX := SCREENWIDTH div 2 - 320;
  nY := SCREENHEIGHT div 2 - 238;

  m_EdNewId := TEdit.Create(frmMain.Owner);
  with m_EdNewId do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 116;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;

  m_EdNewPasswd := TEdit.Create(frmMain.Owner);
  with m_EdNewPasswd do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 137;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    PasswordChar := '*';
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdConfirm := TEdit.Create(frmMain.Owner);
  with m_EdConfirm do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 158;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    PasswordChar := '*';
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdYourName := TEdit.Create(frmMain.Owner);
  with m_EdYourName do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 187;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 20;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdSSNo := TEdit.Create(frmMain.Owner);
  with m_EdSSNo do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 207;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 14;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdBirthDay := TEdit.Create(frmMain.Owner);
  with m_EdBirthDay do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 227;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdQuiz1 := TEdit.Create(frmMain.Owner);
  with m_EdQuiz1 do begin
    Parent := frmMain;
    Height := 16;
    Width := 163;
    Left := nX + 161;
    Top := nY + 256;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 20;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdAnswer1 := TEdit.Create(frmMain.Owner);
  with m_EdAnswer1 do begin
    Parent := frmMain;
    Height := 16;
    Width := 163;
    Left := nX + 161;
    Top := nY + 276;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 12;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdQuiz2 := TEdit.Create(frmMain.Owner);
  with m_EdQuiz2 do begin
    Parent := frmMain;
    Height := 16;
    Width := 163;
    Left := nX + 161;
    Top := nY + 297;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 20;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdAnswer2 := TEdit.Create(frmMain.Owner);
  with m_EdAnswer2 do begin
    Parent := frmMain;
    Height := 16;
    Width := 163;
    Left := nX + 161;
    Top := nY + 317;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 12;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdPhone := TEdit.Create(frmMain.Owner);
  with m_EdPhone do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 347;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 14;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdMobPhone := TEdit.Create(frmMain.Owner);
  with m_EdMobPhone do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 368;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 13;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;
  m_EdEMail := TEdit.Create(frmMain.Owner);
  with m_EdEMail do begin
    Parent := frmMain;
    Height := 16;
    Width := 116;
    Left := nX + 161;
    Top := nY + 388;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 40;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 11;
  end;

  nX := SCREENWIDTH div 2 - 210 {192} {192};
  nY := SCREENHEIGHT div 2 - 150 {146} {150};
  m_EdChgId := TEdit.Create(frmMain.Owner);
  with m_EdChgId do begin
    Parent := frmMain;
    Height := 16;
    Width := 137;
    Left := nX + 239;
    Top := nY + 117;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 12;
  end;
  m_EdChgCurrentpw := TEdit.Create(frmMain.Owner);
  with m_EdChgCurrentpw do begin
    Parent := frmMain;
    Height := 16;
    Width := 137;
    Left := nX + 239;
    Top := nY + 149;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    PasswordChar := '*';
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 12;
  end;
  m_EdChgNewPw := TEdit.Create(frmMain.Owner);
  with m_EdChgNewPw do begin
    Parent := frmMain;
    Height := 16;
    Width := 137;
    Left := nX + 239;
    Top := nY + 176;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    PasswordChar := '*';
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 12;
  end;
  m_EdChgRepeat := TEdit.Create(frmMain.Owner);
  with m_EdChgRepeat do begin
    Parent := frmMain;
    Height := 16;
    Width := 137;
    Left := nX + 239;
    Top := nY + 208;
    BorderStyle := bsNone;
    Color := clblack;
    Font.Color := clWhite;
    MaxLength := 10;
    PasswordChar := '*';
    Visible := False;
    OnKeyPress := EdNewIdKeyPress;
    OnEnter := EdNewOnEnter;
    tag := 12;
  end;
end;

destructor TLoginScene.Destroy;
begin
  inherited Destroy;
end;

procedure TLoginScene.OpenScene;
var
  i: Integer;
begin
  m_nCurFrame := 0;
  m_nMaxFrame := 10;
  m_sLoginId := '';
  m_sLoginPasswd := '';

  {with m_EdPasswd do begin
    Left := SCREENWIDTH div 2 - 24 - 0;
    Top := SCREENHEIGHT div 2 - 51 - 0;
    Height := 16;
    Width := 136;
    Visible := False;
  end;}

  m_boOpenFirst := True;

  FrmDlg.DLogin.Visible := True;
  FrmDlg.DNewAccount.Visible := False;
  m_boNowOpening := False;
  g_SndMgr.PlayBKGSound(bmg_intro);

end;

procedure TLoginScene.CloseScene;
begin
  FrmDlg.DLogin.Visible := False;
  g_SndMgr.SilenceSound;
end;

procedure TLoginScene.EdLoginIdKeyPress(Sender: TObject; var Key: Char);
begin
  if byte(Key) = VK_RETURN then begin
    Key := #0;
    m_sLoginId := LowerCase(FrmDlg.DxEditLoginID.Caption);
    if m_sLoginId <> '' then
      SetDFocus(FrmDlg.DxEditPassword);
  end else if byte(Key) = VK_TAB then begin
    Key := #0;
    SetDFocus(FrmDlg.DxEditPassword);
  end;
end;

procedure TLoginScene.EdLoginPasswdKeyPress(Sender: TObject; var Key: Char);
var
   Msg: TDefaultMessage;
  sSend,EnStr: string;
begin

  if (Key = '~') or (Key = '''') then Key := '_';
  if byte(Key) = VK_RETURN then begin
    Key := #0;
    m_sLoginId := LowerCase(FrmDlg.DxEditLoginID.Caption);
    m_sLoginPasswd := FrmDlg.DxEditPassword.Caption;
    if (m_sLoginId <> '') and (m_sLoginPasswd <> '') then begin
      frmMain.LoginID := m_sLoginId;
      frmMain.LoginPasswd := m_sLoginPasswd;
      try
         frmMain.SendLogin (frmMain.LoginID , frmMain.LoginPasswd);
      finally
        g_boSendLogin := True;
        FrmDlg.DxEditLoginID.Caption := '';
        FrmDlg.DxEditPassword.Caption := '';
      end;
    end else if (FrmDlg.DxEditLoginID.Visible) and (FrmDlg.DxEditLoginID.Caption = '') then
      SetDFocus(FrmDlg.DxEditLoginID);
  end else if byte(Key) = VK_TAB then begin
    Key := #0;
    SetDFocus(FrmDlg.DxEditLoginID);
  end;
end;

procedure TLoginScene.PassWdFail;
begin
  SetDFocus(FrmDlg.DxEditLoginID);
end;

function TLoginScene.NewIdCheckNewId: Boolean;
begin
  Result := True;
  m_EdNewId.Text := Trim(m_EdNewId.Text);
  if Length(m_EdNewId.Text) < 3 then begin
    FrmDlg.DMessageDlg('登录帐号的长度必须大于3位.', [mbOk]);
    Beep;
    m_EdNewId.SetFocus;
    Result := False;
  end;
end;

function TLoginScene.NewIdCheckSSno: Boolean;
var
  Str, t1, t2, t3, syear, smon, sday: string;
  ayear, amon, aday, sex: Integer;
  flag: Boolean;
begin
  Result := True;
  Str := m_EdSSNo.Text;
  Str := GetValidStr3(Str, t1, ['-']);
  GetValidStr3(Str, t2, ['-']);
  flag := True;
  if (Length(t1) = 6) and (Length(t2) = 7) then begin
    smon := Copy(t1, 3, 2);
    sday := Copy(t1, 5, 2);
    amon := Str_ToInt(smon, 0);
    aday := Str_ToInt(sday, 0);
    if (amon <= 0) or (amon > 12) then flag := False;
    if (aday <= 0) or (aday > 31) then flag := False;
    sex := Str_ToInt(Copy(t2, 1, 1), 0);
    if (sex <= 0) or (sex > 2) then flag := False;
  end else flag := False;
  if not flag then begin
    Beep;
    m_EdSSNo.SetFocus;
    Result := False;
  end;
end;

function TLoginScene.NewIdCheckBirthDay: Boolean;
var
  Str, t1, t2, t3, syear, smon, sday: string;
  ayear, amon, aday, sex: Integer;
  flag: Boolean;
begin
  Result := True;
  flag := True;
  Str := m_EdBirthDay.Text;
  Str := GetValidStr3(Str, syear, ['/']);
  Str := GetValidStr3(Str, smon, ['/']);
  Str := GetValidStr3(Str, sday, ['/']);
  ayear := Str_ToInt(syear, 0);
  amon := Str_ToInt(smon, 0);
  aday := Str_ToInt(sday, 0);
  if (ayear <= 1890) or (ayear > 2101) then flag := False;
  if (amon <= 0) or (amon > 12) then flag := False;
  if (aday <= 0) or (aday > 31) then flag := False;
  if not flag then begin
    Beep;
    m_EdBirthDay.SetFocus;
    Result := False;
  end;
end;

procedure TLoginScene.EdNewIdKeyPress(Sender: TObject; var Key: Char);
var
  Str, t1, t2, t3, syear, smon, sday: string;
  ayear, amon, aday, sex: Integer;
  flag: Boolean;
begin
  if (Sender = m_EdNewPasswd) or (Sender = m_EdChgNewPw) or (Sender = m_EdChgRepeat) then
    if (Key = '~') or (Key = '''') or (Key = ' ') then Key := #0;
  if Key = #13 then begin
    Key := #0;
    if Sender = m_EdNewId then begin
      if not NewIdCheckNewId then
        Exit;
    end;
    if Sender = m_EdNewPasswd then begin
      if Length(m_EdNewPasswd.Text) < 4 then begin
        FrmDlg.DMessageDlg('密码长度必须大于 4位.', [mbOk]);
        Beep;
        m_EdNewPasswd.SetFocus;
        Exit;
      end;
    end;
    if Sender = m_EdConfirm then begin
      if m_EdNewPasswd.Text <> m_EdConfirm.Text then begin
        FrmDlg.DMessageDlg('二次输入的密码不一至！！！', [mbOk]);
        Beep;
        m_EdConfirm.SetFocus;
        Exit;
      end;
    end;
    if (Sender = m_EdYourName) or (Sender = m_EdQuiz1) or (Sender = m_EdAnswer1) or
      (Sender = m_EdQuiz2) or (Sender = m_EdAnswer2) or (Sender = m_EdPhone) or
      (Sender = m_EdMobPhone) or (Sender = m_EdEMail) then begin
      TEdit(Sender).Text := Trim(TEdit(Sender).Text);
      if TEdit(Sender).Text = '' then begin
        Beep;
        TEdit(Sender).SetFocus;
        Exit;
      end;
    end;
    if (Sender = m_EdSSNo) and (not EnglishVersion) then begin //茄惫牢 版快.. 林刮殿废锅龋 埃帆 盲农
      if not NewIdCheckSSno then
        Exit;
    end;
    if Sender = m_EdBirthDay then begin
      if not NewIdCheckBirthDay then
        Exit;
    end;
    if TEdit(Sender).Text <> '' then begin
      if Sender = m_EdNewId then m_EdNewPasswd.SetFocus;
      if Sender = m_EdNewPasswd then m_EdConfirm.SetFocus;
      if Sender = m_EdConfirm then m_EdYourName.SetFocus;
      if Sender = m_EdYourName then m_EdSSNo.SetFocus;
      if Sender = m_EdSSNo then m_EdBirthDay.SetFocus;
      if Sender = m_EdBirthDay then m_EdQuiz1.SetFocus;
      if Sender = m_EdQuiz1 then m_EdAnswer1.SetFocus;
      if Sender = m_EdAnswer1 then m_EdQuiz2.SetFocus;
      if Sender = m_EdQuiz2 then m_EdAnswer2.SetFocus;
      if Sender = m_EdAnswer2 then m_EdPhone.SetFocus;
      if Sender = m_EdPhone then m_EdMobPhone.SetFocus;
      if Sender = m_EdMobPhone then m_EdEMail.SetFocus;
      if Sender = m_EdEMail then begin
        if m_EdNewId.Enabled then m_EdNewId.SetFocus
        else if m_EdNewPasswd.Enabled then m_EdNewPasswd.SetFocus;
      end;

      if Sender = m_EdChgId then m_EdChgCurrentpw.SetFocus;
      if Sender = m_EdChgCurrentpw then m_EdChgNewPw.SetFocus;
      if Sender = m_EdChgNewPw then m_EdChgRepeat.SetFocus;
      if Sender = m_EdChgRepeat then m_EdChgId.SetFocus;
    end;
  end;
end;

procedure TLoginScene.EdNewOnEnter(Sender: TObject);
var
  hx, hy: Integer;
begin
  FrmDlg.NAHelps.Clear;
  hx := TEdit(Sender).Left + TEdit(Sender).Width + 10;
  hy := TEdit(Sender).Top + TEdit(Sender).Height - 18;
  if Sender = m_EdNewId then begin
    FrmDlg.NAHelps.Add('您的帐号名称可以包括：');
    FrmDlg.NAHelps.Add('字符、数字的组合。');
    FrmDlg.NAHelps.Add('帐号名称长度必须为4或以上。');
    FrmDlg.NAHelps.Add('登陆帐号并游戏中的人物名称。');
    FrmDlg.NAHelps.Add('请仔细输入创建帐号所需信息。');
    FrmDlg.NAHelps.Add('您的登陆帐号可以登陆游戏');
    FrmDlg.NAHelps.Add('及我们网站，以取得一些相关信息。');
    FrmDlg.NAHelps.Add('');
    FrmDlg.NAHelps.Add('建议您的登陆帐号不要与游戏中的角');
    FrmDlg.NAHelps.Add('色名相同，');
    FrmDlg.NAHelps.Add('以确保你的密码不会被爆力破解。');
  end;
  if Sender = m_EdNewPasswd then begin
    FrmDlg.NAHelps.Add('您的密码可以是字符及数字的组合，');
    FrmDlg.NAHelps.Add('但密码长度必须至少4位。');
    FrmDlg.NAHelps.Add('建议您的密码内容不要过于简单，');
    FrmDlg.NAHelps.Add('以防被人猜到。');
    FrmDlg.NAHelps.Add('请记住您输入的密码，如果丢失密码');
    FrmDlg.NAHelps.Add('将无法登录游戏。');
    FrmDlg.NAHelps.Add('');
    FrmDlg.NAHelps.Add('');
    FrmDlg.NAHelps.Add('');
    FrmDlg.NAHelps.Add('');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdConfirm then begin
    FrmDlg.NAHelps.Add('再次输入密码');
    FrmDlg.NAHelps.Add('以确认。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdYourName then begin
    FrmDlg.NAHelps.Add('请输入您的全名.');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdSSNo then begin
    FrmDlg.NAHelps.Add('请输入你的身份证号');
    FrmDlg.NAHelps.Add('例如： 720101-146720');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdBirthDay then begin
    FrmDlg.NAHelps.Add('请输入您的生日');
    FrmDlg.NAHelps.Add('例如：1977/10/15');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdQuiz1 then begin
    FrmDlg.NAHelps.Add('请输入第一个密码提示问题');
    FrmDlg.NAHelps.Add('这个提示将用于密码丢失后找');
    FrmDlg.NAHelps.Add('回密码用。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdAnswer1 then begin
    FrmDlg.NAHelps.Add('请输入上面问题的');
    FrmDlg.NAHelps.Add('答案。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdQuiz2 then begin
    FrmDlg.NAHelps.Add('请输入第二个密码提示问题');
    FrmDlg.NAHelps.Add('这个提示将用于密码丢失后找');
    FrmDlg.NAHelps.Add('回密码用。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdAnswer2 then begin
    FrmDlg.NAHelps.Add('请输入上面问题的');
    FrmDlg.NAHelps.Add('答案。');
    FrmDlg.NAHelps.Add('');
  end;
  if (Sender = m_EdYourName) or (Sender = m_EdSSNo) or (Sender = m_EdQuiz1) or (Sender = m_EdQuiz2) or (Sender = m_EdAnswer1) or (Sender = m_EdAnswer2) then begin
    FrmDlg.NAHelps.Add('您输入的信息必须真实正确的信息');
    FrmDlg.NAHelps.Add('如果使用了虚假的注册信息');
    FrmDlg.NAHelps.Add('您的帐号将被取消。');
    FrmDlg.NAHelps.Add('');
  end;

  if Sender = m_EdPhone then begin
    FrmDlg.NAHelps.Add('请输入您的电话');
    FrmDlg.NAHelps.Add('号码。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdMobPhone then begin
    FrmDlg.NAHelps.Add('请输入您的手机号码。');
    FrmDlg.NAHelps.Add('');
  end;
  if Sender = m_EdEMail then begin
    FrmDlg.NAHelps.Add('请输入您的邮件地址。您的邮件将被');
    FrmDlg.NAHelps.Add('接收最近更新的一些信息');
    FrmDlg.NAHelps.Add('');
  end;
end;

procedure TLoginScene.HideLoginBox;
begin
  ChangeLoginState(lsCloseAll);
end;

procedure TLoginScene.OpenLoginDoor;
begin
  m_boNowOpening := True;
  m_dwStartTime := GetTickCount;
  HideLoginBox;
  g_SndMgr.PlaySound(s_rock_door_open);
end;

procedure TLoginScene.PlayScene(MSurface: TCustomCanvas);
var
  d: TCustomLockableTexture;
begin
  if m_boOpenFirst then begin
    m_boOpenFirst := False;
    SetDFocus(FrmDlg.DxEditLoginID);
  end;

  d := g_WChrSelImages.Images[22 {381}];

  if d <> nil then begin
    MSurface.Draw((SCREENWIDTH - 800) div 2, (SCREENHEIGHT - 600) div 2, d.ClientRect, d, False);
  end;
  if m_boNowOpening then begin
    if GetTickCount - m_dwStartTime > 28 then begin
      m_dwStartTime := GetTickCount;
      Inc(m_nCurFrame);
    end;
    if m_nCurFrame >= m_nMaxFrame - 1 then begin
      m_nCurFrame := m_nMaxFrame - 1;
      if not g_boDoFadeOut and not g_boDoFadeIn then begin
        g_boDoFadeOut := True;
        g_boDoFadeIn := True;
        g_nFadeIndex := 29;
      end;
    end;
    d := g_WChrSelImages.Images[103 + m_nCurFrame - 80];
    if d <> nil then MSurface.Draw((SCREENWIDTH - 800) div 2 + 152, (SCREENHEIGHT - 600) div 2 + 96, d.ClientRect, d, True);
    if g_boDoFadeOut then begin
      if g_nFadeIndex <= 1 then begin
        g_WMainImages.ClearCache;
        g_WChrSelImages.ClearCache;
        //if g_Logined then FrmDlg.DscStart.tag := FrmDlg.DscStart.WLib.Images[FrmDlg.DscStart.FaceIndex].Height;        //ASP注释
        if g_Logined then FrmDlg.DscStart.tag := FrmDlg.DscStart.WLib.Images[g_sDscStart0 + byte(FrmDlg.DscStart.Downed)].Height;
        if not g_boDoFadeOut and not g_boDoFadeIn then begin
          //g_boDoFadeOut := True;
          g_boDoFadeIn := True;
          g_nFadeIndex := 0;
        end;
        DScreen.ChangeScene(stSelectChr);
      end;
    end;
  end;
end;

procedure TLoginScene.ChangeLoginState(State: TLoginState);
var
  i, focus: Integer;
  C: TControl;
begin
  focus := -1;
  case State of
    lsLogin: focus := 10;
    lsNewidRetry, lsNewid: focus := 11;
    lsChgpw: focus := 12;
    lsCloseAll: focus := -1;
  end;
  with frmMain do begin //login
    for i := 0 to ControlCount - 1 do begin
      C := Controls[i];
      if C is TEdit then begin
        if C.tag in [10..12] then begin
          if C.tag = focus then begin
            C.Visible := True;
            TEdit(C).Text := '';
          end else begin
            C.Visible := False;
            TEdit(C).Text := '';
          end;
        end;
      end;
    end;
    if EnglishVersion then
      m_EdSSNo.Visible := False;

    case State of
      lsLogin: begin
          FrmDlg.DNewAccount.Visible := False;
          FrmDlg.DChgPw.Visible := False;
          FrmDlg.DLogin.Visible := True;
          if FrmDlg.DxEditLoginID.Visible then
            SetDFocus(FrmDlg.DxEditLoginID);
        end;
      lsNewidRetry,
        lsNewid: begin
          //if m_boUpdateAccountMode then
          //  m_EdNewId.Enabled := False
          //else
          m_EdNewId.Enabled := True;
          FrmDlg.DNewAccount.Visible := True;
          FrmDlg.DChgPw.Visible := False;
          FrmDlg.DLogin.Visible := False;
          if m_EdNewId.Visible and m_EdNewId.Enabled then begin
            m_EdNewId.SetFocus;
          end else begin
            if m_EdConfirm.Visible and m_EdConfirm.Enabled then
              m_EdConfirm.SetFocus;
          end;
        end;
      lsChgpw: begin
          FrmDlg.DNewAccount.Visible := False;
          FrmDlg.DChgPw.Visible := True;
          FrmDlg.DLogin.Visible := False;
          if m_EdChgId.Visible then m_EdChgId.SetFocus;
        end;
      lsCloseAll: begin
          FrmDlg.DNewAccount.Visible := False;
          FrmDlg.DChgPw.Visible := False;
          FrmDlg.DLogin.Visible := False;
        end;
    end;
  end;
end;

procedure TLoginScene.NewClick;
begin
  //m_boUpdateAccountMode := False;
  FrmDlg.NewAccountTitle := '';
  ChangeLoginState(lsNewid);
end;

procedure TLoginScene.NewIdRetry(boupdate: Boolean);
begin
  //m_boUpdateAccountMode := boupdate;
  ChangeLoginState(lsNewidRetry);
  m_EdNewId.Text := m_NewIdRetryUE.sAccount;
  m_EdNewPasswd.Text := m_NewIdRetryUE.sPassword;
  m_EdYourName.Text := m_NewIdRetryUE.sUserName;
  m_EdSSNo.Text := m_NewIdRetryUE.sSSNo;
  m_EdQuiz1.Text := m_NewIdRetryUE.sQuiz;
  m_EdAnswer1.Text := m_NewIdRetryUE.sAnswer;
  m_EdPhone.Text := m_NewIdRetryUE.sPhone;
  m_EdEMail.Text := m_NewIdRetryUE.sEMail;
  m_EdQuiz2.Text := m_NewIdRetryUE.sQuiz2;
  m_EdAnswer2.Text := m_NewIdRetryUE.sAnswer2;
  m_EdMobPhone.Text := m_NewIdRetryUE.sMobilePhone;
  m_EdBirthDay.Text := m_NewIdRetryUE.sBirthDay;
end;

procedure TLoginScene.UpdateAccountInfos(ue: TUserEntryA);
begin
  m_NewIdRetryUE := ue;
  FillChar(m_NewIdRetryUE, SizeOf(TUserEntryA), #0);
  //m_boUpdateAccountMode := True;
  NewIdRetry(True);
  FrmDlg.NewAccountTitle := '(请填写帐号相关信息)';
end;

procedure TLoginScene.OkClick;
var
  Key: Char;
begin
  Key := #13;
  EdLoginPasswdKeyPress(Self, Key);
end;

procedure TLoginScene.ChgPwClick;
begin
  ChangeLoginState(lsChgpw);
end;

function TLoginScene.CheckUserEntrys: Boolean;
begin
  Result := False;
  m_EdNewId.Text := Trim(m_EdNewId.Text);
  m_EdQuiz1.Text := Trim(m_EdQuiz1.Text);
  m_EdYourName.Text := Trim(m_EdYourName.Text);
  if not NewIdCheckNewId then Exit;

  if not EnglishVersion then begin
    if not NewIdCheckSSno then
      Exit;
  end;

  if not NewIdCheckBirthDay then Exit;
  if Length(m_EdNewId.Text) < 3 then begin
    m_EdNewId.SetFocus;
    Exit;
  end;
  if Length(m_EdNewPasswd.Text) < 3 then begin
    m_EdNewPasswd.SetFocus;
    Exit;
  end;
  if m_EdNewPasswd.Text <> m_EdConfirm.Text then begin
    m_EdConfirm.SetFocus;
    Exit;
  end;
  if Length(m_EdQuiz1.Text) < 1 then begin
    m_EdQuiz1.SetFocus;
    Exit;
  end;
  if Length(m_EdAnswer1.Text) < 1 then begin
    m_EdAnswer1.SetFocus;
    Exit;
  end;
  if Length(m_EdQuiz2.Text) < 1 then begin
    m_EdQuiz2.SetFocus;
    Exit;
  end;
  if Length(m_EdAnswer2.Text) < 1 then begin
    m_EdAnswer2.SetFocus;
    Exit;
  end;
  if Length(m_EdYourName.Text) < 1 then begin
    m_EdYourName.SetFocus;
    Exit;
  end;
  if not EnglishVersion then begin
    if Length(m_EdSSNo.Text) < 1 then begin
      m_EdSSNo.SetFocus;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TLoginScene.NewAccountOk;
var
  ue: TUserEntryA;
begin
  if CheckUserEntrys then begin
    FillChar(ue, SizeOf(TUserEntry), #0);
    ue.sAccount := LowerCase(m_EdNewId.Text);
    ue.sPassword := m_EdNewPasswd.Text;
    ue.sUserName := m_EdYourName.Text;

    if not EnglishVersion then
      ue.sSSNo := m_EdSSNo.Text
    else
      ue.sSSNo := '650101-1455111';

    ue.sQuiz := m_EdQuiz1.Text;
    ue.sAnswer := Trim(m_EdAnswer1.Text);
    ue.sPhone := m_EdPhone.Text;
    ue.sEMail := Trim(m_EdEMail.Text);

    ue.sQuiz2 := m_EdQuiz2.Text;
    ue.sAnswer2 := Trim(m_EdAnswer2.Text);
    ue.sBirthDay := m_EdBirthDay.Text;
    ue.sMobilePhone := m_EdMobPhone.Text;

    m_NewIdRetryUE := ue;
    m_NewIdRetryUE.sAccount := '';
    m_NewIdRetryUE.sPassword := '';

    //if not m_boUpdateAccountMode then
    frmMain.SendNewAccount(ue);
    //else
    //  frmMain.SendUpdateAccount(ue, ua);
    //m_boUpdateAccountMode := False;
    NewAccountClose;
  end;
end;

procedure TLoginScene.NewAccountClose;
begin
  //if not m_boUpdateAccountMode then
  ChangeLoginState(lsLogin);
end;

procedure TLoginScene.ChgpwOk;
var
  uid, passwd, newpasswd: string;
begin
  if m_EdChgNewPw.Text = m_EdChgRepeat.Text then begin
    uid := m_EdChgId.Text;
    passwd := m_EdChgCurrentpw.Text;
    newpasswd := m_EdChgNewPw.Text;
    frmMain.SendChgPw(uid, passwd, newpasswd);
    ChgpwCancel;
  end else begin
    FrmDlg.DMessageDlg('二次输入的密码不匹配！！！。', [mbOk]);
    m_EdChgNewPw.SetFocus;
  end;
end;

procedure TLoginScene.ChgpwCancel;
begin
  ChangeLoginState(lsLogin);
end;

{-------------------- TSelectChrScene ------------------------}

constructor TSelectChrScene.Create;
begin
  CreateChrMode := False;
  FillChar(ChrArr, SizeOf(TSelChar) * 2, #0);
  ChrArr[0].FreezeState := True;
  ChrArr[1].FreezeState := True;
  NewIndex := 0;

  SoundTimer := TTimer.Create(frmMain.Owner);
  with SoundTimer do begin
    OnTimer := SoundOnTimer;
    Interval := 1;
    Enabled := False;
  end;
  inherited Create(stSelectChr);
end;

destructor TSelectChrScene.Destroy;
begin
  inherited Destroy;
end;

procedure TSelectChrScene.OpenScene;
begin
  FrmDlg.DSelectChr.Visible := True;
  SoundTimer.Enabled := True;
  SoundTimer.Interval := 1;
end;

procedure TSelectChrScene.CloseScene;
begin
  g_SndMgr.SilenceSound;
  FrmDlg.DSelectChr.Visible := False;
  SoundTimer.Enabled := False;
end;

procedure TSelectChrScene.SoundOnTimer(Sender: TObject);
begin
  g_SndMgr.PlayBKGSound(bmg_select);
  SoundTimer.Enabled := False;
end;

procedure TSelectChrScene.SelChrSelect1Click;
begin
  if (not ChrArr[0].Selected) and (ChrArr[0].Valid) and ChrArr[0].FreezeState then begin
    frmMain.SelectChr(ChrArr[0].UserChr.Name); //2004/05/17
    ChrArr[0].Selected := True;
    ChrArr[1].Selected := False;
    ChrArr[0].Unfreezing := True;
    ChrArr[0].AniIndex := 0;
    ChrArr[0].DarkLevel := 0;
    ChrArr[0].EffIndex := 0;
    ChrArr[0].StartTime := GetTickCount;
    ChrArr[0].moretime := GetTickCount;
    ChrArr[0].startefftime := GetTickCount;
    g_SndMgr.PlaySound(s_meltstone);
  end;
end;

procedure TSelectChrScene.SelChrSelect2Click;
begin
  if (not ChrArr[1].Selected) and (ChrArr[1].Valid) and ChrArr[1].FreezeState then begin
    frmMain.SelectChr(ChrArr[1].UserChr.Name); //2004/05/17
    ChrArr[1].Selected := True;
    ChrArr[0].Selected := False;
    ChrArr[1].Unfreezing := True;
    ChrArr[1].AniIndex := 0;
    ChrArr[1].DarkLevel := 0;
    ChrArr[1].EffIndex := 0;
    ChrArr[1].StartTime := GetTickCount;
    ChrArr[1].moretime := GetTickCount;
    ChrArr[1].startefftime := GetTickCount;
    g_SndMgr.PlaySound(s_meltstone);
  end;
end;

procedure TSelectChrScene.SelChrStartClick;
var
  chrname: string;
begin
{$IFNDEF TEST}
  if FrmDlg.DscStart.tag > 0 then Exit;
{$ENDIF}
  chrname := '';
  if ChrArr[0].Valid and ChrArr[0].Selected then chrname := ChrArr[0].UserChr.Name;
  if ChrArr[1].Valid and ChrArr[1].Selected then chrname := ChrArr[1].UserChr.Name;
  if chrname <> '' then begin
    if not g_boDoFadeOut and not g_boDoFadeIn then begin
      g_boDoFastFadeOut := True;
      g_nFadeIndex := 29;
    end;
    frmMain.SendSelChr(chrname);
  end else
    FrmDlg.DMessageDlg('开始游戏前你应该先创建一个新角色！\点击<创建角色>按钮创建一个游戏角色。', [mbOk]);
end;

procedure TSelectChrScene.SelChrNewChrClick;
begin
  if not ChrArr[0].Valid or not ChrArr[1].Valid then begin
    if not ChrArr[0].Valid then
      MakeNewChar(0)
    else
      MakeNewChar(1);
  end else
    FrmDlg.DMessageDlg('一个帐号最多只能创建 2 个游戏角色！', [mbOk]);
end;

procedure TSelectChrScene.SelChrEraseChrClick;
var
  n: Integer;
begin
  n := 0;
  if ChrArr[0].Valid and ChrArr[0].Selected then n := 0;
  if ChrArr[1].Valid and ChrArr[1].Selected then n := 1;
  if (ChrArr[n].Valid) and (not ChrArr[n].FreezeState) and (ChrArr[n].UserChr.Name <> '') then begin
    if mrYes = FrmDlg.DMessageDlg('"' + ChrArr[n].UserChr.Name + '" 是否确认删除此游戏角色？', [mbYes, mbNo]) then
      frmMain.SendDelChr(ChrArr[n].UserChr.Name);
  end;
end;

procedure TSelectChrScene.SelChrCreditsClick;
begin

  //[失败] 没有找到被删除的角色。
  //[失败] 客户端版本错误。
  //[失败] 你没有这个角色。
  //[失败] 角色已被删除。
  //[失败] 角色数据读取失败，请稍候再试。
  //[失败] 你选择的服务器用户满员。
  {if not ChrArr[0].Valid or not ChrArr[1].Valid then begin
    if not ChrArr[0].Valid then
      MakeNewChar(0)
    else
      MakeNewChar(1);
  end else
    FrmDlg.DMessageDlg('一个帐号最多只能创建二个游戏角色！', [mbOk]);}
end;

procedure TSelectChrScene.SelChrExitClick;
begin
  frmMain.Close;
end;

procedure TSelectChrScene.ClearChrs;
begin
  FillChar(ChrArr, SizeOf(TSelChar) * 2, #0);
  ChrArr[0].FreezeState := False;
  ChrArr[1].FreezeState := True; //扁夯捞 倔绢 乐绰 惑怕
  ChrArr[0].Selected := True;
  ChrArr[1].Selected := False;
  ChrArr[0].UserChr.Name := '';
  ChrArr[1].UserChr.Name := '';
end;

procedure TSelectChrScene.AddChr(uname: string; job, hair, Level, sex: Integer);
var
  n: Integer;
begin
  if not ChrArr[0].Valid then
    n := 0
  else if not ChrArr[1].Valid then
    n := 1
  else
    Exit;
  ChrArr[n].UserChr.Name := uname;
  ChrArr[n].UserChr.job := job;
  ChrArr[n].UserChr.hair := hair;
  ChrArr[n].UserChr.Level := Level;
  ChrArr[n].UserChr.sex := sex;
  ChrArr[n].Valid := True;
end;

procedure TSelectChrScene.MakeNewChar(Index: Integer);
var
  nx, ny: Integer;
begin
  //以下新增  20200712
  nx := (SCREENWIDTH - 800) div 2;
  ny := (SCREENHEIGHT - 600) div 2;
  //以上新增
  CreateChrMode := True;
  NewIndex := Index;
  if Index = 0 then begin
    FrmDlg.DCreateChr.Left := 415 + nx;
    FrmDlg.DCreateChr.Top := 15 + ny;
  end else begin
    FrmDlg.DCreateChr.Left := 75 + nx;
    FrmDlg.DCreateChr.Top := 15 + ny;
  end;
  FrmDlg.DCreateChr.Visible := True;
  ChrArr[NewIndex].Valid := True;
  ChrArr[NewIndex].FreezeState := False;
  //EdChrName.Left := FrmDlg.DCreateChr.Left + 71;
  //EdChrName.Top := FrmDlg.DCreateChr.Top + 107;
  //EdChrName.Visible := True;
  FrmDlg.DxEdChrName.SetFocus;
  SelectChr(NewIndex);
  FillChar(ChrArr[NewIndex].UserChr, SizeOf(TUserCharacterInfo), #0);
end;

{procedure TSelectChrScene.EdChrnameKeyPress(Sender: TObject; var Key: Char);
begin

end;}

procedure TSelectChrScene.SelectChr(Index: Integer);
begin
  ChrArr[Index].Selected := True;
  ChrArr[Index].DarkLevel := 30;
  ChrArr[Index].StartTime := GetTickCount;
  ChrArr[Index].moretime := GetTickCount;
  if Index = 0 then
    ChrArr[1].Selected := False
  else
    ChrArr[0].Selected := False;
end;

procedure TSelectChrScene.SelChrNewClose;
begin
  ChrArr[NewIndex].Valid := False;
  CreateChrMode := False;
  FrmDlg.DCreateChr.Visible := False;
  //EdChrName.Visible := False;
  ChrArr[NewIndex].Selected := True;
  ChrArr[NewIndex].FreezeState := False;
end;

procedure TSelectChrScene.SelChrNewOk;
var
  chrname, shair, sjob, ssex: string;
begin
  chrname := Trim(FrmDlg.DxEdChrName.Text);
  if chrname <> '' then begin
    ChrArr[NewIndex].Valid := False;
    CreateChrMode := False;
    FrmDlg.DCreateChr.Visible := False;
    //EdChrName.Visible := False;
    ChrArr[NewIndex].Selected := True;
    ChrArr[NewIndex].FreezeState := False;
    shair := IntToStr(1 + Random(5)); //////****IntToStr(ChrArr[NewIndex].UserChr.Hair);
    sjob := IntToStr(ChrArr[NewIndex].UserChr.job);
    ssex := IntToStr(ChrArr[NewIndex].UserChr.sex);
    frmMain.SendNewChr(frmMain.LoginID, chrname, shair, sjob, ssex); //货 某腐磐甫 父电促.
  end;
  
end;

procedure TSelectChrScene.SelChrNewJob(job: Integer);
begin
  if (job in [0..2]) and (ChrArr[NewIndex].UserChr.job <> job) then begin
    ChrArr[NewIndex].UserChr.job := job;
    SelectChr(NewIndex);
  end;
end;

procedure TSelectChrScene.SelChrNewm_btSex(sex: Integer);
begin
  if sex <> ChrArr[NewIndex].UserChr.sex then begin
    ChrArr[NewIndex].UserChr.sex := sex;
    SelectChr(NewIndex);
  end;
end;

procedure TSelectChrScene.SelChrNewPrevHair;
begin
end;

procedure TSelectChrScene.SelChrNewNextHair;
begin
end;

procedure TSelectChrScene.PlayScene(MSurface: TCustomCanvas);
var
  n, bx, by, fx, fy, img: Integer;
  ex, ey: Integer; //选择人物时显示的效果光位置
  d, E, dd: TCustomLockableTexture;
  svname: string;
  AspFont : TCustomTextureFont;
begin
  if g_boOpenAutoPlay and (g_nAPReLogon = 2) then begin //0613
    if GetTickCount - g_nAPReLogonWaitTick > g_nAPReLogonWaitTime then begin
      g_nAPReLogonWaitTick := GetTickCount;
      g_nAPReLogon := 3;
      if not g_boDoFadeOut and not g_boDoFadeIn then begin
        g_boDoFastFadeOut := True;
        g_nFadeIndex := 29;
      end;
      frmMain.SendSelChr(frmMain.m_sCharName);
    end;
  end;

  bx := 0;
  by := 0;
  fx := 0;
  fy := 0;
{$IF Var_UI = Var_176}
  d := g_WMainImages.Images[65];
  if d <> nil then begin
    MSurface.Draw((SCREENWIDTH - d.Width) div 2, (SCREENHEIGHT - d.Height) div 2, d.ClientRect, d, False);
  end;
{$ELSE}
  d := g_WMain3Images.Images[400];
  if d <> nil then begin
    MSurface.Draw((SCREENWIDTH - d.Width) div 2, (SCREENHEIGHT - d.Height) div 2, d.ClientRect, d, False);
  end;
{$IFEND}

  for n := 0 to 1 do begin
    if ChrArr[n].Valid then begin
      ex := (SCREENWIDTH - 800) div 2 + 90 {90};
      ey := (SCREENHEIGHT - 600) div 2 + 60 - 2 {60-2};
      case ChrArr[n].UserChr.job of
        0: begin
            if ChrArr[n].UserChr.sex = 0 then begin
              bx := (SCREENWIDTH - 800) div 2 + 71 {71};
              by := (SCREENHEIGHT - 600) div 2 + 75 - 23 {75-23};
              fx := bx;
              fy := by;
            end else begin
              bx := (SCREENWIDTH - 800) div 2 + 65 {65};
              by := (SCREENHEIGHT - 600) div 2 + 75 - 2 - 18;
              fx := bx - 28 + 28;
              fy := by - 16 + 16;
            end;
          end;
        1: begin
            if ChrArr[n].UserChr.sex = 0 then begin
              bx := (SCREENWIDTH - 800) div 2 + 77 {77};
              by := (SCREENHEIGHT - 600) div 2 + 75 - 29;
              fx := bx;
              fy := by;
            end else begin
              bx := (SCREENWIDTH - 800) div 2 + 141 + 30;
              by := (SCREENHEIGHT - 600) div 2 + 85 + 14 - 2;
              fx := bx - 30;
              fy := by - 14;
            end;
          end;
        2: begin
            if ChrArr[n].UserChr.sex = 0 then begin
              bx := (SCREENWIDTH - 800) div 2 + 85;
              by := (SCREENHEIGHT - 600) div 2 + 75 - 12;
              fx := bx;
              fy := by;
            end else begin
              bx := (SCREENWIDTH - 800) div 2 + 141 + 23;
              by := (SCREENHEIGHT - 600) div 2 + 85 + 20 - 2;
              fx := bx - 23;
              fy := by - 20;
            end;
          end;
      end;
      if n = 1 then begin
        ex := (SCREENWIDTH - 800) div 2 + 430;
        ey := (SCREENHEIGHT - 600) div 2 + 60;
        bx := bx + 340;
        by := by + 2;
        fx := fx + 340;
        fy := fy + 2;
      end;
      if ChrArr[n].Unfreezing then begin
        img := 140 - 80 + ChrArr[n].UserChr.job * 40 + ChrArr[n].UserChr.sex * 120;
        d := g_WChrSelImages.Images[img + ChrArr[n].AniIndex];
        E := g_WChrSelImages.Images[4 + ChrArr[n].EffIndex];
        if d <> nil then MSurface.Draw(bx, by, d.ClientRect, d, True);
        if E <> nil then MSurface.DrawBlend(ex, ey, E, 1);
        if GetTickCount - ChrArr[n].StartTime > 110 then begin
          ChrArr[n].StartTime := GetTickCount;
          ChrArr[n].AniIndex := ChrArr[n].AniIndex + 1;
        end;
        if GetTickCount - ChrArr[n].startefftime > 110 then begin
          ChrArr[n].startefftime := GetTickCount;
          ChrArr[n].EffIndex := ChrArr[n].EffIndex + 1;
        end;
        if ChrArr[n].AniIndex > FREEZEFRAME - 1 then begin
          ChrArr[n].Unfreezing := False;
          ChrArr[n].FreezeState := False;
          ChrArr[n].AniIndex := 0;
        end;
      end else if not ChrArr[n].Selected and (not ChrArr[n].FreezeState and not ChrArr[n].Freezing) then begin
        ChrArr[n].Freezing := True;
        ChrArr[n].AniIndex := 0;
        ChrArr[n].StartTime := GetTickCount;
      end;
      if ChrArr[n].Freezing then begin
        img := 140 - 80 + ChrArr[n].UserChr.job * 40 + ChrArr[n].UserChr.sex * 120;
        d := g_WChrSelImages.Images[img + FREEZEFRAME - ChrArr[n].AniIndex - 1];
        if d <> nil then MSurface.Draw(bx, by, d.ClientRect, d, True);
        if GetTickCount - ChrArr[n].StartTime > 110 then begin
          ChrArr[n].StartTime := GetTickCount;
          ChrArr[n].AniIndex := ChrArr[n].AniIndex + 1;
        end;
        if ChrArr[n].AniIndex > FREEZEFRAME - 1 then begin
          ChrArr[n].Freezing := False;
          ChrArr[n].FreezeState := True;
          ChrArr[n].AniIndex := 0;
        end;
      end;
      if not ChrArr[n].Unfreezing and not ChrArr[n].Freezing then begin
        if not ChrArr[n].FreezeState then begin
          img := 120 - 80 + ChrArr[n].UserChr.job * 40 + ChrArr[n].AniIndex + ChrArr[n].UserChr.sex * 120;
          d := g_WChrSelImages.Images[img];
          if d <> nil then begin
//            if ChrArr[n].DarkLevel > 0 then begin
//              dd := TDirectDrawSurface.Create(frmMain.DXDraw.DDraw);
//              dd.SetSize(d.Width, d.Height);
//              dd.Draw(0, 0, d.ClientRect, d, False);
//              MakeDark(dd, 30 - ChrArr[n].DarkLevel);
//              MSurface.Draw(fx, fy, dd.ClientRect, dd, True);
//              dd.free;
//            end else
              MSurface.Draw(fx, fy, d.ClientRect, d, True);
          end;
        end else begin
          img := 140 - 80 + ChrArr[n].UserChr.job * 40 + ChrArr[n].UserChr.sex * 120;
          d := g_WChrSelImages.Images[img];
          if d <> nil then
            MSurface.Draw(bx, by, d.ClientRect, d, True);
        end;
        if ChrArr[n].Selected then begin
          if GetTickCount - ChrArr[n].StartTime > 230 then begin
            ChrArr[n].StartTime := GetTickCount;
            ChrArr[n].AniIndex := ChrArr[n].AniIndex + 1;
            if ChrArr[n].AniIndex > SELECTEDFRAME - 1 then
              ChrArr[n].AniIndex := 0;
          end;
          if GetTickCount - ChrArr[n].moretime > 25 then begin
            ChrArr[n].moretime := GetTickCount;
            if ChrArr[n].DarkLevel > 0 then
              ChrArr[n].DarkLevel := ChrArr[n].DarkLevel - 1;
          end;
        end;
      end;
      //显示选择角色时人物名称等级
      if n = 0 then begin
        if ChrArr[n].UserChr.Name <> '' then begin
          with MSurface do begin
            BoldText((SCREENWIDTH - 800) div 2 + 117 {117}, (SCREENHEIGHT - 600) div 2 + 492 + 2 {492+2}, ChrArr[n].UserChr.Name, clWhite, clblack);
            BoldText((SCREENWIDTH - 800) div 2 + 117 {117}, (SCREENHEIGHT - 600) div 2 + 523 {523}, IntToStr(ChrArr[n].UserChr.Level), clWhite, clblack);
            BoldText((SCREENWIDTH - 800) div 2 + 117 {117}, (SCREENHEIGHT - 600) div 2 + 553 {553}, GetJobName(ChrArr[n].UserChr.job), clWhite, clblack);
          end;
        end;
      end else begin
        if ChrArr[n].UserChr.Name <> '' then begin
          with MSurface do begin
            BoldText((SCREENWIDTH - 800) div 2 + 671 {671}, (SCREENHEIGHT - 600) div 2 + 492 + 4 {492+4}, ChrArr[n].UserChr.Name, clWhite, clblack);
            BoldText((SCREENWIDTH - 800) div 2 + 671 {671}, (SCREENHEIGHT - 600) div 2 + 525 {525}, IntToStr(ChrArr[n].UserChr.Level), clWhite, clblack);
            BoldText((SCREENWIDTH - 800) div 2 + 671 {671}, (SCREENHEIGHT - 600) div 2 + 555 {555}, GetJobName(ChrArr[n].UserChr.job), clWhite, clblack);
          end;
        end;
      end;
      with MSurface do begin
        AspFont := FontManager.GetFont('宋体',9,[]);
        svname := g_sServerName;
        BoldText(SCREENWIDTH div 2 {405} - AspFont.TextWidth(svname) div 2, (SCREENHEIGHT - 600) div 2 + 8 {8}, svname, clWhite, clblack);
      end;
    end;
  end;
end;

{--------------------------- TLoginNotice ----------------------------}

constructor TLoginNotice.Create;
begin
  inherited Create(stLoginNotice);
end;

destructor TLoginNotice.Destroy;
begin
  inherited Destroy;
end;

end.

