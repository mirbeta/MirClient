{************************************************************************}
{ HTMLDialog component                                                   }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{          copyright © 2001-2013                                         }
{           Email : info@tmssoftware.com                                 }
{           Web : http://www.tmssoftware.com                             }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit HTMLDialog;

{$I TMSDEFS.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HTMLabel, StdCtrls, PictureContainer;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.0 : New : Support for Delphi 2006 & C++Builder 2006
  // 1.2.0.0 : Improved : property persistence in the DFM file
  //         : New functions HTMLShowDialog, HTMLShowDialogPos, HTMLMessageDlgXXX
  //         : New OnAnchorHint event added
  // 1.3.0.0 : New : support for Autosizing dialog buttons
  // 1.3.0.1 : Fixed : issue with LeftButton.Visible
  // 1.4.0.0 : New : Support for PNG images via images in associated PictureContainer

type

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;
  TAnchorHintEvent = procedure (Sender:TObject; var Anchor:string) of object;

  TButtonProperties = class(TPersistent)
  private
    { Private declarations }
    FVisible: boolean;
    FCaption: string;
    FModalResult: TModalResult;
    FCancel: boolean;
    FDefault: boolean;
  published
    { Published declarations }
    property Cancel: boolean read FCancel write FCancel default false;
    property Caption: string read FCaption write FCaption;
    property Default: boolean read FDefault write FDefault default false;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property Visible: boolean read FVisible write FVisible;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLDialog = class(TComponent)
  private
    { Private declarations }
    FCaption: string;
    FColor: TColor;
    FImages: TImageList;
    FHTMLText: TStringList;
    FFont: TFont;
    FHover: boolean;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FURLColor: TColor;
    FAnchorExit: TAnchorClick;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorHint: TAnchorHintEvent;
    FLeftButton:TButtonProperties;
    FCenterButton:TButtonProperties;
    FRightButton:TButtonProperties;
    FPosition: TPosition;
    FHeight: integer;
    FWidth: integer;
    FContainer: TPictureContainer;
    FDialogLeft: integer;
    FDialogTop: integer;
    procedure SetHTMLText(const Value: TStringList);
    procedure SetImages(const Value: TImageList);
    procedure SetFont(const Value: TFont);
    procedure SetCenterButton(const Value: TButtonProperties);
    procedure SetLeftButton(const Value: TButtonProperties);
    procedure SetRightButton(const Value: TButtonProperties);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function ShowModal:integer;
    property DialogLeft: integer read FDialogLeft write FDialogLeft;
    property DialogTop: integer read FDialogTop write FDialogTop;
  published
    { Published declarations }
    property Caption:string read FCaption write FCaption;
    property CenterButton:TButtonProperties read FCenterButton write SetCenterButton;
    property Color:TColor read FColor write FColor default clBtnFace;
    property Font:TFont read FFont write SetFont;
    property Height:integer read FHeight write FHeight;
    property Hover:boolean read FHover write FHover default false;
    property HoverColor:TColor read FHoverColor write FHoverColor default clNone;
    property HoverFontColor:TColor read FHoverFontColor write FHoverFontColor default clNone;
    property HTMLText:TStringList read FHTMLText write SetHTMLText;
    property Images:TImageList read FImages write SetImages;
    property LeftButton:TButtonProperties read FLeftButton write SetLeftButton;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property Position:TPosition read FPosition write FPosition;
    property RightButton:TButtonProperties read FRightButton write SetRightButton;
    property ShadowColor:TColor read FShadowColor write FShadowColor default clGray;
    property ShadowOffset:Integer read FShadowOffset write FShadowOffset default 1;
    property URLColor:TColor read FURLColor write FURLColor default clBlue;
    property Width:integer read FWidth write FWidth;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnAnchorHint: TAnchorHintEvent read FAnchorHint write FAnchorHint;
    property Version: string read GetVersion write SetVersion;
  end;

procedure HTMLShowMessage(const Value: string);
procedure HTMLShowMessagePos(const Value: string; X, Y: integer);

function HTMLMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function HTMLMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function HTMLMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;

function HTMLMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;




implementation

uses
  Consts;

{ THTMLDialog }

constructor THTMLDialog.Create(aOwner: TComponent);
begin
  inherited;
  FImages := nil;
  FHTMLText := TStringList.Create;
  FFont := TFont.Create;
  FURLColor := clBlue;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FHoverColor := clNone;
  FHoverFontColor := clNone;
  FCenterButton := TButtonProperties.Create;
  FLeftButton := TButtonProperties.Create;
  FRightButton := TButtonProperties.Create;

  FCenterButton.Caption := 'Ok';
  FCenterButton.Default := true;
  FCenterButton.Visible := true;
  FCenterButton.ModalResult := mrOk;

  FRightButton.Caption := 'Cancel';
  FRightButton.Cancel := true;
  FRightButton.Visible := true;
  FRightButton.ModalResult := mrCancel;

  FPosition := poScreenCenter;

  FWidth := 180;
  FHeight := 90;
  FColor := clBtnFace;
end;

destructor THTMLDialog.Destroy;
begin
  FHTMLText.Free;
  FFont.Free;
  FCenterButton.Free;
  FLeftButton.Free;
  FRightButton.Free;
  inherited;
end;


procedure THTMLDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation=opRemove) and (AComponent=FImages) then FImages:=nil;

  if (AOperation=opRemove) and (AComponent=FContainer) then FContainer:=nil;  
  inherited;
end;

procedure THTMLDialog.SetCenterButton(const Value: TButtonProperties);
begin
  FCenterButton.Assign(Value);
end;

procedure THTMLDialog.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure THTMLDialog.SetHTMLText(const Value: TStringList);
begin
  FHTMLText.Assign(Value);
end;

procedure THTMLDialog.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

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


procedure THTMLDialog.SetLeftButton(const Value: TButtonProperties);
begin
  FLeftButton.Assign(Value);
end;

procedure THTMLDialog.SetRightButton(const Value: TButtonProperties);
begin
  FRightButton.Assign(Value);
end;

function THTMLDialog.ShowModal: integer;
var
  Form: TForm;
  HTMLabel: THTMLabel;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight, BW, NBW: Integer;
  OffsX, OffsY: Integer;

begin
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := FCaption;

      Color := FColor;

      ClientWidth := FWidth;
      ClientHeight := FHeight;
      Position := FPosition;

      if Position = poDesigned then
      begin
        Left := DialogLeft;
        Top := DialogTop;
      end;

      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      ButtonTop := ClientHeight - ButtonHeight -4;

      HTMLabel := THTMLabel.Create(Form);
      HTMLabel.PictureContainer := self.PictureContainer;

      with HTMLabel do
      begin
        Parent := Form;
        Font.Assign(FFont);
        OffsX := MulDiv(8, DialogUnits.X, 4);
        OffsY := MulDiv(8, DialogUnits.Y, 8);

        Left := OffsX;
        Top := OffsY;
        Width := Parent.Width - 2* OffsX;

        Images := FImages;
        HTMLText.Assign(FHTMLText);
        Hover := FHover;
        HoverFontColor := FHoverFontColor;
        HoverColor := FHoverColor;
        URLColor := FURLColor;
        ShadowColor := FShadowColor;
        ShadowOffset := FShadowOffset;
        AnchorHint := true;
        OnAnchorClick := FAnchorClick;
        OnAnchorEnter := FAnchorEnter;
        OnAnchorExit := FAnchorExit;
        OnAnchorHint := FAnchorHint;

        AutoSizing := True;
      end;

      BW := 0;
      NBW := 0;

      if FRightButton.Visible then
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := FRightButton.Caption;
        Default := FRightButton.Default;
        ModalResult := FRightButton.ModalResult;
        Cancel := FRightButton.Cancel;

        NBW := Canvas.TextWidth(FRightButton.Caption) + 16;

        if NBW < ButtonWidth then
          NBW := ButtonWidth;

        SetBounds(Form.ClientWidth - (NBW + 10), ButtonTop, NBW, ButtonHeight);
      end;

      BW := BW + (NBW + 10);
      NBW := 0;

      if FCenterButton.Visible then
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := FCenterButton.Caption;
        ModalResult := FCenterButton.ModalResult;
        Default := FCenterButton.Default;
        Cancel := FCenterButton.Cancel;

        NBW := Canvas.TextWidth(FCenterButton.Caption) + 16;

        if NBW < ButtonWidth then
          NBW := ButtonWidth;

        SetBounds(Form. ClientWidth - BW - (NBW + 10), ButtonTop, NBW, ButtonHeight);
      end;

      BW := BW + (NBW + 10);

      if FLeftButton.Visible then
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := FLeftButton.Caption;
        ModalResult := FLeftButton.ModalResult;
        Default := FLeftButton.Default;
        Cancel := FLeftButton.Cancel;
        NBW := Canvas.TextWidth(FLeftButton.Caption) + 16;

        if NBW < ButtonWidth then
          NBW := ButtonWidth;

        SetBounds(Form.ClientWidth - BW -  (NBW + 10), ButtonTop, NBW, ButtonHeight);
      end;

      Result := ShowModal;

    finally
      Form.Free;
    end;
end;

function THTMLDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLDialog.SetVersion(const Value: string);
begin

end;


procedure HTMLShowMessageInt(const Value: string; X,Y: integer; Position: TPosition);
var
  hd: THTMLDialog;
begin
  hd := THTMLDialog.Create(Application.MainForm);
  hd.HTMLText.Text := Value;
  hd.CenterButton.Visible := true;
  hd.CenterButton.Caption := 'OK';
  hd.CenterButton.ModalResult := mrOK;
  hd.Position := Position;
  hd.DialogLeft := X;
  hd.DialogTop := Y;
  hd.Caption := Application.Title;
  hd.RightButton.Visible := false;
  try
    hd.ShowModal;
  finally
    hd.Free;
  end;

end;

procedure HTMLShowMessage(const Value: string);
begin
  HTMLShowMessageInt(Value,0,0,poScreenCenter);
end;

procedure HTMLShowMessagePos(const Value: string; X,Y: integer);
begin
  HTMLShowMessageInt(Value,X,Y,poDesigned);
end;

function HTMLMessageDlgPosInt(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn; Position: TPosition): Integer; overload;
var
  d: THTMLDialog;
begin
  d := THTMLDialog.Create(Application.MainForm);

  d.CenterButton.Visible := false;
  d.RightButton.Visible := false;
  d.LeftButton.Visible := false;

  if mbOK in Buttons then
    with d.CenterButton do
    begin
      Visible := true;
      Caption := SMsgDlgOK;
      ModalResult := mrOK;
    end;
  if mbCancel in Buttons then
    with d.RightButton do
    begin
      Visible := true;
      Caption := SMsgDlgCancel;
      ModalResult := mrCancel;
    end;
  if mbYES in Buttons then
    with d.CenterButton do
    begin
      Visible := true;
      Caption := SMsgDlgYes;
      ModalResult := mrYes;
    end;
  if mbNO in Buttons then
    with d.RightButton do
    begin
      Visible := true;
      Caption := SMsgDlgNo;
      ModalResult := mrNo;
    end;
  if mbAbort in Buttons then
    with d.LeftButton do
    begin
      Visible := true;
      Caption := SMsgDlgAbort;
      ModalResult := mrAbort;
    end;
  if mbRetry in Buttons then
    with d.CenterButton do
    begin
      Visible := true;
      Caption := SMsgDlgRetry;
      ModalResult := mrRetry;
     end;
  if mbIgnore in Buttons then
    with d.RightButton do
    begin
      Visible := true;
      Caption := SMsgDlgIgnore;
      ModalResult := mrIgnore;
    end;
  if mbHelp in Buttons then
    with d.LeftButton do
    begin
      Visible := true;
      Caption := 'Help';
    end;

  d.Caption := Title;
  d.HTMLText.Text := Msg;
  d.DialogLeft := X;
  d.DialogTop := Y;
  d.Position := Position;
  try
    Result := d.ShowModal;
  finally
    d.Free;
  end;
end;



function HTMLMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := HTMLMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, 0, 0, mbOK, poScreenCenter);
end;

function HTMLMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := HTMLMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, 0, 0, DefaultButton, poScreenCenter);
end;

function HTMLMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;
begin
  Result := HTMLMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, X,Y, mbOK, poDesigned);
end;

function HTMLMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := HTMLMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, X,Y, DefaultButton, poDesigned);
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}
  


end.
