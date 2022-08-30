{***************************************************************************}
{ TMS Metro Dialogs                                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvMetroDlgs;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvMetroForm, AdvStyleIF, HTMLabel, AdvMetroRes;

procedure MetroShowMessage(Text: string); overload;
procedure MetroShowMessage(Text, Caption: string); overload;
procedure MetroShowMessage(Text, Caption, CaptionText: string); overload;
procedure MetroShowMessageFmt(const Instruction: string; Parameters: array of const);

function MetroMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function MetroMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function MetroMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT): Integer;

function MetroInputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;


implementation

uses
  AdvGlowButton, AdvEdit, AdvGDIP, Math, Consts, AdvGDIPicture;

const
  TXTMARGIN_X = 20;
  TXTMARGIN_Y = 20;
  CTRLSPACING = 10;

  {$IFNDEF DELPHI_UNICODE}
  mrClose    = mrYesToAll + 1;
  {$ENDIF}

type
  TBtnHandler = class(TObject)
    procedure BtnClick(Sender: TObject);
  end;

function HTMLEncode(const Data: string): string;
var
  res: string;

begin
  if pos('</',Data) > 0 then
    Result := Data
  else
  begin
    res := Data;
    res := StringReplace(res, '<','&lt;',[rfReplaceAll]);
    res := StringReplace(res, '>','&gt;',[rfReplaceAll]);
    res := StringReplace(res, '<','&amp;',[rfReplaceAll]);
    res := StringReplace(res, '<','&quot;',[rfReplaceAll]);
    Result := StringReplace(res, #13#10,'<br>',[rfReplaceAll]);
  end;
end;

procedure TBtnHandler.BtnClick(Sender: TObject);
begin
  ((Sender as TAdvGlowButton).Parent as TCustomForm).Tag := (Sender as TAdvGlowButton).ModalResult;
  ((Sender as TAdvGlowButton).Parent as TCustomForm).Close;
end;

procedure MetroShowMessage(Text: string);
begin
  MetroShowMessage(Text,Application.MainForm.Caption,'');
end;

procedure MetroShowMessage(Text, Caption: string);
begin
  MetroShowMessage(Text,Caption,'');
end;


procedure MetroShowMessage(Text, Caption, CaptionText: string);
var
  MF: TAdvMetroForm;
  HL: THTMLabel;
  AGB: TAdvGlowButton;
  BtnHandler: TBtnHandler;

begin
  GlowSpeed := 0;

  if IsClearTones(MetroFormTones) then
    MetroFormTones := DefaultMetroTones;

  BtnHandler := TBtnHandler.Create;

  MF := TAdvMetroForm.CreateNew(Application);
  MF.Height := 250;
  MF.Width := 300;
  MF.Left := 500;
  MF.Caption := Caption;
  MF.Text := CaptionText;
  MF.SizeGrip := False;
  MF.BorderIcons := [biSystemMenu];
  MF.BorderStyle := bsNone;

  {$IFDEF DELPHI2007_LVL}
  MF.Padding.Left := TXTMARGIN_X;
  MF.Padding.Top := TXTMARGIN_Y;
  MF.Padding.Right := TXTMARGIN_X;
  MF.Padding.Bottom := TXTMARGIN_Y;
  {$ENDIF}

  HL := THTMLabel.Create(MF);
  HL.Align := alClient;
  HL.Parent := MF;
  HL.Font.Color := MetroFormTones.Background.TextColor;
  HL.Font.Name := GetMetroFont;
  HL.HTMLText.Text := HTMLEncode(Text);
  HL.AutoSizeType := asBoth;
  HL.AutoSizing := true;
  MF.SetColorTones(MetroFormTones);

  MF.Width := Max(200, Min(hl.HTMLSize.cx + 2*TXTMARGIN_X, GetSystemMetrics(SM_CXSCREEN) - 2 * TXTMARGIN_X));
  MF.Height := hl.HTMLSize.cy + 32 + 32 + 2 * TXTMARGIN_Y;

  AGB := TAdvGlowButton.Create(MF);
  AGB.Parent := MF;
  AGB.Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y;
  AGB.Left := (MF.Width - 60) div 2;
  AGB.Width := 60;
  AGB.Height := 26;
  AGB.Default := true;
  AGB.OnClick := BtnHandler.BtnClick;

  AGB.SetColorTones(MetroFormTones);

  AGB.Font.Name := GetMetroFont;
  AGB.Caption := SOKButton;
  MF.Position := poScreenCenter;
  MF.Tag := 0;
  MF.ShowModal;

  BtnHandler.Free;
  MF.Free;
end;

function MetroMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT): Integer;
const
  MB_CANCELTRYCONTINUE = $00000006;  // missing from windows unit so probably never be used

var
  MF: TAdvMetroForm;
  HL: THTMLabel;
  clrt: TColorTones;
  BtnHandler: TBtnHandler;
  BtnList: TList;
  i,cx: integer;

  function CreateButton(Text:string; ModalResult: TModalResult): TAdvGlowButton;
  begin
    Result := TAdvGlowButton.Create(MF);
    Result.Caption := Text;
    Result.Parent := MF;
    Result.OnClick := BtnHandler.BtnClick;
    Result.ModalResult := ModalResult;
    Result.Font.Name := GetMetroFont;
    Result.SetColorTones(clrt);
    Result.Default := ModalResult = mrOK;
    Result.Cancel := ModalResult = mrCancel;
  end;

begin
  GlowSpeed := 0;

  BtnHandler := TBtnHandler.Create;
  BtnList := TList.Create;

  if IsClearTones(MetroFormTones) then
    MetroFormTones := DefaultMetroTones;

  MF := TAdvMetroForm.CreateNew(Application);
  MF.Height := 250;
  MF.Width := 300;
  MF.Left := 500;
  MF.Caption := lpTitle;
  MF.Text := '';
  MF.SizeGrip := False;
  MF.BorderIcons := [biSystemMenu];
  MF.BorderStyle := bsNone;

  {$IFDEF DELPHI2007_LVL}
  MF.Padding.Left := TXTMARGIN_X;
  MF.Padding.Top := TXTMARGIN_Y;
  MF.Padding.Right := TXTMARGIN_X;
  MF.Padding.Bottom := TXTMARGIN_Y;
  {$ENDIF}

  HL := THTMLabel.Create(MF);
  HL.Align := alClient;
  HL.Parent := MF;
  HL.Font.Color := MetroFormTones.Background.TextColor;
  HL.Font.Name := GetMetroFont;
  HL.HTMLText.Text := HTMLEncode(lpInstruction);
  HL.AutoSizeType := asBoth;
  HL.AutoSizing := true;

  MF.Width := Max(200, Min(hl.HTMLSize.cx + 2*TXTMARGIN_X, GetSystemMetrics(SM_CXSCREEN) - 2 * TXTMARGIN_X));
  MF.Height := hl.HTMLSize.cy + 32 + 32 + 2 * TXTMARGIN_Y;

  MF.SetColorTones(MetroFormTones);

  case MB_TYPEMASK and flags of
    MB_ABORTRETRYIGNORE:
      begin
        BtnList.Add(CreateButton(SAbortButton, mrAbort));
        BtnList.Add(CreateButton(SRetryButton, mrRetry));
        BtnList.Add(CreateButton(SIgnoreButton, mrIgnore));
      end;
    MB_CANCELTRYCONTINUE:
      begin
        BtnList.Add(CreateButton(SCancelButton, mrCancel));
        BtnList.Add(CreateButton(SRetryButton, mrRetry));
        BtnList.Add(CreateButton(SContinue, mrClose));
      end;
    MB_OK:
      begin
        BtnList.Add(CreateButton(SOKButton, mrOK));
      end;
    MB_RETRYCANCEL:
      begin
        BtnList.Add(CreateButton(SRetryButton, mrRetry));
        BtnList.Add(CreateButton(SCancelButton, mrCancel));
      end;
    MB_OKCANCEL:
      begin
        BtnList.Add(CreateButton(SOKButton, mrOK));
        BtnList.Add(CreateButton(SCancelButton, mrCancel));
      end;
    MB_YESNOCANCEL:
      begin
        BtnList.Add(CreateButton(SYesButton, mrYes));
        BtnList.Add(CreateButton(SNoButton, mrNo));
        BtnList.Add(CreateButton(SCancelButton, mrCancel));
      end;
    MB_YESNO:
      begin
        BtnList.Add(CreateButton(SYesButton, mrYes));
        BtnList.Add(CreateButton(SNoButton, mrNo));
      end;
  end;

  cx := MF.Width - TXTMARGIN_X;

  for i := BtnList.Count - 1 downto 0 do
  begin
    TAdvGlowButton(BtnList.Items[i]).Left := cx - 60;
    TAdvGlowButton(BtnList.Items[i]).Width := 60;
    TAdvGlowButton(BtnList.Items[i]).Height := 26;
    TAdvGlowButton(BtnList.Items[i]).Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y;
    TAdvGlowButton(BtnList.Items[i]).SetColorTones(MetroFormTones);
    cx := cx - 60 - 10;
  end;

  MF.Position := poScreenCenter;
  MF.Tag := 0;
  MF.ShowModal;

  BtnList.Free;
  BtnHandler.Free;
  Result := MF.Tag;
  MF.Free;
end;

procedure MetroShowMessageFmt(const Instruction: string; Parameters: array of const);
begin
  MetroShowmessage(Format(Instruction,Parameters));
end;

function MetroMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := MetroMessageDlg(Instruction, DlgType, Buttons, HelpCtx, mbHelp);
end;

function MetroMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
var
  MF: TAdvMetroForm;
  HL: THTMLabel;
  pic: TAdvGDIPPicture;
  clrt: TColorTones;
  BtnHandler: TBtnHandler;
  BtnList: TList;
  i, cx, imgsz: integer;
  IcoStr: string;

  function CreateButton(Text:string; ModalResult: TModalResult): TAdvGlowButton;
  begin
    Result := TAdvGlowButton.Create(MF);
    Result.Caption := Text;
    Result.Parent := MF;
    Result.OnClick := BtnHandler.BtnClick;
    Result.ModalResult := ModalResult;
    Result.Font.Name := GetMetroFont;
    Result.SetColorTones(clrt);
  end;

begin
  GlowSpeed := 0;

  BtnHandler := TBtnHandler.Create;
  BtnList := TList.Create;

  if IsClearTones(MetroFormTones) then
    MetroFormTones := DefaultMetroTones;

  MF := TAdvMetroForm.CreateNew(Application);
  MF.Height := 250;
  MF.Width := 300;
  MF.Left := 500;
  MF.SizeGrip := False;
  MF.BorderIcons := [biSystemMenu];
  MF.SetColorTones(MetroFormTones);
  MF.BorderStyle := bsNone;

  {$IFDEF DELPHI2007_LVL}
  MF.Padding.Left := TXTMARGIN_X;
  MF.Padding.Top := TXTMARGIN_Y;
  MF.Padding.Right := TXTMARGIN_X;
  MF.Padding.Bottom := TXTMARGIN_Y;
  {$ENDIF}

  HL := THTMLabel.Create(MF);
  HL.Align := alClient;
  HL.Parent := MF;
  HL.Font.Color := MetroFormTones.Background.TextColor;
  HL.Font.Name := GetMetroFont;
  HL.HTMLText.Text := '<br>' + HTMLEncode(Instruction);
  HL.AutoSizeType := asBoth;
  HL.AutoSizing := true;

  imgsz := 0;
  if DlgType <> mtCustom then
    imgsz := 48;

  MF.Width := Max(200, Min(hl.HTMLSize.cx + imgsz + 2 * TXTMARGIN_X, GetSystemMetrics(SM_CXSCREEN) - 2 * TXTMARGIN_X));

  MF.Height := hl.HTMLSize.cy + 32 + 32 + 2 * TXTMARGIN_Y;

  case DlgType of
    mtWarning:
      begin
        MF.Caption := SMsgDlgWarning;
        icoStr := 'METRO_EXCLAMATION';
      end;
    mtError:
      begin
        MF.Caption := SMsgDlgError;
        icoStr := 'METRO_ERROR';
      end;
    mtInformation:
      begin
        MF.Caption := SMsgDlgInformation;
        icoStr := 'METRO_INFO';
      end;
    mtConfirmation:
      begin
        MF.Caption := SMsgDlgConfirm;
        icoStr := 'METRO_QUESTION';
      end;
  end;

  if DlgType <> mtCustom then
  begin
    Pic := TAdvGDIPPicture.Create(MF);
    Pic.Parent := MF;
    Pic.AutoSize := true;
    Pic.Picture.LoadFromResourceName(HInstance,icoStr);
    pic.BackgroundColor := clRed;
    Pic.Align := alLeft;
  end;

  if (mbYes in Buttons) then
    BtnList.Add(CreateButton(SYesButton, mrYes));

  if (mbNo in Buttons) then
    BtnList.Add(CreateButton(SNoButton, mrNo));

  if (mbOK in Buttons) then
    BtnList.Add(CreateButton(SOKButton, mrOK));

  if (mbCancel in Buttons) then
    BtnList.Add(CreateButton(SCancelButton, mrCancel));

  if (mbAbort in Buttons) then
    BtnList.Add(CreateButton(SCloseButton, mrClose));

  if (mbRetry in Buttons) then
    BtnList.Add(CreateButton(SRetryButton, mrRetry));

  if (mbIgnore in Buttons) then
    BtnList.Add(CreateButton(SMsgDlgIgnore, mrIgnore));

  if (mbAll in Buttons) then
    BtnList.Add(CreateButton(SMsgDlgAll, mrAll));

  if (mbNoToAll in buttons) then
    BtnList.Add(CreateButton(SMsgDlgNoToAll, mrNoToAll));

  if (mbYesToAll in buttons) then
    BtnList.Add(CreateButton(SMsgDlgYesToAll, mrYesToAll));

  if (mbHelp in buttons) then
    BtnList.Add(CreateButton(SMsgDlgHelp, mrNone));


  cx := MF.Width - TXTMARGIN_X;

  for i := BtnList.Count - 1 downto 0 do
  begin
    TAdvGlowButton(BtnList.Items[i]).Left := cx - 60;
    TAdvGlowButton(BtnList.Items[i]).Width := 60;
    TAdvGlowButton(BtnList.Items[i]).Height := 26;
    TAdvGlowButton(BtnList.Items[i]).Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y;
    TAdvGlowButton(BtnList.Items[i]).SetColorTones(MetroFormTones);
    cx := cx - 60 - 10;
  end;

  MF.Position := poScreenCenter;
  MF.Tag := 0;
  MF.ShowModal;

  BtnList.Free;
  BtnHandler.Free;
  Result := MF.Tag;
  MF.Free;
end;

function MetroInputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;
var
  MF: TAdvMetroForm;
  HL: THTMLabel;
  BtnHandler: TBtnHandler;
  DlgOK,DlgCancel: TAdvGlowButton;
  AEdit: TAdvEdit;

  function CreateButton(Text:string; ModalResult: TModalResult): TAdvGlowButton;
  begin
    Result := TAdvGlowButton.Create(MF);
    Result.Caption := Text;
    Result.Parent := MF;
    Result.OnClick := BtnHandler.BtnClick;
    Result.ModalResult := ModalResult;
    Result.Font.Name := GetMetroFont;
    Result.SetColorTones(MetroFormTones);
  end;

begin
  GlowSpeed := 0;

  BtnHandler := TBtnHandler.Create;

  if IsClearTones(MetroFormTones) then
    MetroFormTones := DefaultMetroTones;

  MF := TAdvMetroForm.CreateNew(Application);
  MF.Height := 250;
  MF.Width := 300;
  MF.Left := 500;
  MF.Caption := ACaption;
  MF.SizeGrip := False;
  MF.BorderIcons := [biSystemMenu];
  MF.SetColorTones(MetroFormTones);
  MF.BorderStyle := bsNone;

  {$IFDEF DELPHI2007_LVL}
  MF.Padding.Left := TXTMARGIN_X;
  MF.Padding.Top := TXTMARGIN_Y;
  MF.Padding.Right := TXTMARGIN_X;
  MF.Padding.Bottom := TXTMARGIN_Y;
  {$ENDIF}

  HL := THTMLabel.Create(MF);
  HL.Align := alClient;
  HL.Parent := MF;
  HL.Font.Color := MetroFormTones.Background.TextColor;
  HL.Font.Name := GetMetroFont;
  HL.HTMLText.Text := HTMLEncode(APrompt);
  HL.AutoSizeType := asBoth;
  HL.AutoSizing := true;

  AEdit := TAdvEdit.Create(MF);
  AEdit.Parent := MF;

  AEdit.BorderColor := MF.Appearance.CaptionActiveColor;
  AEdit.Color := MetroFormTones.Background.BrushColor;
  AEdit.Font.Color := MetroFormTones.Background.TextColor;
  AEdit.FocusFontColor := MetroFormTones.Background.TextColor;
  AEdit.Text := Value;

  MF.Width := Max(200, Min(hl.HTMLSize.cx + 2 * TXTMARGIN_X, GetSystemMetrics(SM_CXSCREEN) - 2 * TXTMARGIN_X));
  MF.Height := hl.HTMLSize.cy + 32 + 32 + 2 * TXTMARGIN_Y + AEdit.Height + CTRLSPACING * 2;

  AEdit.Left := TXTMARGIN_X;
  AEdit.Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y;
  AEdit.Width := MF.Width - 2 * TXTMARGIN_X;
  AEdit.DefaultHandling := false;

  DlgOK := CreateButton(SOKButton, mrOk);
  DlgCancel := CreateButton(SCancelButton, mrCancel);

  DlgOK.Left := (MF.Width div 2) - 60 - CTRLSPACING div 2;
  DlgOK.Width := 60;
  DlgOK.Height := 26;
  DlgOK.Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y * 2 + AEdit.Height;
  DlgOK.Default := true;
  DlgOK.ModalResult := mrOK;

  DlgCancel.Left := (MF.Width div 2) + CTRLSPACING div 2;
  DlgCancel.Width := 60;
  DlgCancel.Height := 26;
  DlgCancel.Cancel := true;
  DlgCancel.ModalResult := mrCancel;
  DlgCancel.Top := HL.Top + hl.HTMLSize.cy + TXTMARGIN_Y * 2 + AEdit.Height;

  MF.Position := poScreenCenter;
  MF.Tag := 0;
  MF.ShowModal;

  BtnHandler.Free;
  Result := (MF.Tag = mrOK);
  if Result then
    Value := AEdit.Text;

  MF.Free;
end;



end.
