{***************************************************************************}
{ Extended dialog components                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2011                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{***************************************************************************}

unit DlgsEx;

{$G+}
{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Consts;

type
  PBoolean = ^Boolean;

function MessageDlgEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  TimeOut: Integer; DoNotShowAgain: PBoolean): Integer;

function MessageDlgPosEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  TimeOut: Integer;  DoNotShowAgain: PBoolean): Integer;

procedure ShowMessageEx(const Msg: string;TimeOut:Integer; DoNotShowAgain: PBoolean);
procedure ShowMessageFmtEx(const Msg: string; Params: array of const;
  TimeOut: Integer; DoNotShowAgain: PBoolean);
procedure ShowMessagePosEx(const Msg: string; X, Y: Integer;
  TimeOut: Integer; DoNotShowAgain: PBoolean);

implementation

type
  TMessageForm = class(TForm)
  private
    FTimer: TTimer;
    FDefaultResult: word;
    FInterval: Integer;
    FCheckBox: TCheckBox;
    procedure HelpButtonClick(Sender: TObject);
    procedure SetInterval(const Value: Integer);
    function GetDoNotShowAgain: Boolean;
  protected
    procedure DoTimer(Sender:TObject);
  public
    constructor CreateNew(AOwner: TComponent);  reintroduce;
    destructor Destroy; override;
  published
    property DefaultResult: word read FDefaultResult write FDefaultResult;
    property Interval: Integer read FInterval write SetInterval;
    property CheckDoNotShowAgain: Boolean read GetDoNotShowAgain;
  end;



constructor TMessageForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 3000;
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
  FCheckBox := nil;
end;

destructor TMessageForm.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TMessageForm.DoTimer(Sender: TObject);
begin
  ModalResult := FDefaultResult;
end;

function TMessageForm.GetDoNotShowAgain: Boolean;
begin
  Result := False;
  if Assigned(FCheckBox) then
    Result := FCheckBox.Checked;
end;

procedure TMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMessageForm.SetInterval(const Value: Integer);
begin
  FInterval := Value;
  FTimer.Interval := Value;
  FTimer.Enabled := Value > 0;
end;

var
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);

  {$IFDEF DELPHI_UNICODE}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help', 'Close');
  {$ELSE}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  ButtonCaptions: array[TMsgDlgBtn] of Pointer = (
    @SMsgDlgYes, @SMsgDlgNo, @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort,
    @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAll, @SMsgDlgNoToAll, @SMsgDlgYesToAll,
    @SMsgDlgHelp, @SMsgDlgClose);
  {$ELSE}
  ButtonCaptions: array[TMsgDlgBtn] of Pointer = (
    @SMsgDlgYes, @SMsgDlgNo, @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort,
    @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAll, @SMsgDlgNoToAll, @SMsgDlgYesToAll,
    @SMsgDlgHelp);
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, 0);
  {$ELSE}
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);
  {$ENDIF}

var
  ButtonWidths : array[TMsgDlgBtn] of integer;  // initialized to zero


function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function Max(a,b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function CreateMessageDialogEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DoNotShowAgain: Boolean): TMessageForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
  mcCheckHeight = 8;

var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, CheckHeight, X, ALeft: Integer;
  B, DefaultButton, CancelButton: TMsgDlgBtn;
  IconID: PChar;
  TextRect: TRect;
begin
  Result := TMessageForm.CreateNew(Application);
  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    CheckHeight := MulDiv(mcCheckHeight, DialogUnits.Y, 8);

    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        if ButtonWidths[B] = 0 then
        begin
          TextRect := Rect(0,0,0,0);
          Windows.DrawText( canvas.handle,
            PChar(LoadResString(ButtonCaptions[B])), -1,
            TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
            DrawTextBiDiModeFlagsReadingOnly);
          with TextRect do ButtonWidths[B] := Right - Left + 8;
        end;
        if ButtonWidths[B] > ButtonWidth then
          ButtonWidth := ButtonWidths[B];
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg)+1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
    IconID := IconIDs[DlgType];
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;

    if DoNotShowAgain then
      ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
        VertMargin * 2 + CheckHeight
    else
      ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
        VertMargin * 2;

    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    
    if DlgType <> mtCustom then
      Caption := LoadResString(Captions[DlgType])
    else
      Caption := Application.Title;

    if IconID <> nil then
      with TImage.Create(Result) do
      begin
        Name := 'Image';
        Parent := Result;
        Picture.Icon.Handle := LoadIcon(0, IconID);
        SetBounds(HorzMargin, VertMargin, 32, 32);
      end;

    with TLabel.Create(Result) do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Right, TextRect.Bottom);
    end;
    if mbOk in Buttons then DefaultButton := mbOk else
      if mbYes in Buttons then DefaultButton := mbYes else
        DefaultButton := mbRetry;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
        with TButton.Create(Result) do
        begin
          Name := ButtonNames[B];
          Parent := Result;
          Caption := LoadResString(ButtonCaptions[B]);
          ModalResult := ModalResults[B];
          if B = DefaultButton then Default := True;
          if B = CancelButton then Cancel := True;

          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);

          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TMessageForm(Result).HelpButtonClick;
        end;

    if DoNotShowAgain then
    begin
      FCheckBox := TCheckBox.Create(Result);
      with FCheckBox do
      begin
        Parent := Result;
        Caption := LoadStr(999);
        SetBounds(ButtonSpacing, IconTextHeight + VertMargin + VertSpacing + ButtonHeight,
           Result.Width, ButtonHeight);
      end;
    end;

    case DefaultButton of
    mbOk: Result.DefaultResult := mrOk;
    mbYes: Result.DefaultResult := mrYes;
    mbRetry: Result.DefaultResult := mrRetry;
    end;
  end;
end;

function MessageDlgPosHelpEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; TimeOut: Integer; DoNotShowAgain: PBoolean): Integer;
var
  DNSA: Boolean;
begin
  DNSA := Assigned(DoNotShowAgain);

  with CreateMessageDialogEx(Msg, DlgType, Buttons, DNSA) do
    try
      Interval := TimeOut;
      HelpContext := HelpCtx;
      HelpFile := HelpFileName;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      if (Y < 0) and (X < 0) then Position := poScreenCenter;
      Result := ShowModal;
      DNSA := CheckDoNotShowAgain;
    finally
      if Assigned(DoNotShowAgain) then
        DoNotShowAgain^ := DNSA;
      Free;
    end;
end;

function MessageDlgEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; TimeOut: Integer; DoNotShowAgain: PBoolean): Integer;
begin
  Result := MessageDlgPosHelpEx(Msg, DlgType, Buttons, HelpCtx, -1, -1, '', TimeOut,DoNotShowAgain);
end;

function MessageDlgPosEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  TimeOut: Integer; DoNotShowAgain: PBoolean): Integer;
begin
  Result := MessageDlgPosHelpEx(Msg, DlgType, Buttons, HelpCtx, X, Y, '', TimeOut, DoNotShowAgain);
end;

procedure ShowMessagePosEx(const Msg: string; X, Y: Integer;
  TimeOut: Integer; DoNotShowAgain: PBoolean);
begin
  MessageDlgPosEx(Msg, mtCustom, [mbOK], 0, X, Y, TimeOut, DoNotShowAgain);
end;

procedure ShowMessageEx(const Msg: string;
  TimeOut:Integer; DoNotShowAgain: PBoolean);
begin
  ShowMessagePosEx(Msg, -1, -1,TimeOut, DoNotShowAgain);
end;

procedure ShowMessageFmtEx(const Msg: string; Params: array of const;
  TimeOut: Integer; DoNotShowAgain: PBoolean);
begin
  ShowMessageEx(Format(Msg, Params), TimeOut, DoNotShowAgain);
end;



end.
