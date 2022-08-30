{************************************************************************}
{ TADVEDIT based Query dialog component                                  }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{          copyright © 2001-2012                                         }
{          Email : info@tmssoftware.com                                  }
{          Web : http://www.tmssoftware.com                              }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit AdvQueryDialog;

{$I TMSDEFS.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvEdit, Consts;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.1.0 : Added property DialogUnitWidth
  //         : Added property DialogColor
  // 1.0.2.0 : New : Exposed ValidChars property

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvQueryDialog = class(TComponent)
  private
    FCaption: string;
    FPrompt: string;
    FEditType: TAdvEditType;
    FPrecision: integer;
    FSuffix: string;
    FPrefix: string;
    FFlat: boolean;
    FText: string;
    FLengthLimit: integer;
    FCanUndo: boolean;
    FShowModified: boolean;
    FShowURL: boolean;
    FModifiedColor: TColor;
    FPasswordChar: char;
    FEditAlign: TEditAlign;
    FSigned: boolean;
    FFlatLineColor: TColor;
    FFlatParentColor: boolean;
    FExcelStyleDecimalSeparator: boolean;
    FShowHint: boolean;
    FHint: string;
    FDialogUnitWidth: integer;
    FDialogColor: TColor;
    FValidChars: string;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    { Private declarations }
  protected
    function GetVersionNr: Integer; virtual;
    { Protected declarations }
  public
    { Public declarations }
    function ShowModal: TModalResult;
    constructor Create(AOwner: TComponent);  override;
  published
    { Published declarations }
    property CanUndo:boolean read FCanUndo write FCanUndo;
    property Caption:string read FCaption write FCaption;
    property DialogUnitWidth: integer read FDialogUnitWidth write FDialogUnitWidth default 180;
    property DialogColor: TColor read FDialogColor write FDialogColor default clNone;
    property EditAlign:TEditAlign read FEditAlign write FEditAlign;
    property EditType:TAdvEditType read FEditType write FEditType;
    property ExcelStyleDecimalSeparator:boolean read FExcelStyleDecimalSeparator write
                                                     FExcelStyleDecimalSeparator;
    property Flat:boolean read FFlat write FFlat default false;
    property FlatLineColor:TColor read FFlatLineColor write FFlatLineColor;
    property FlatParentColor:boolean read FFlatParentColor write FFlatParentColor;
    property Hint:string read FHint write FHint;
    property LengthLimit:integer read FLengthLimit write FLengthLimit;
    property ModifiedColor:TColor read FModifiedColor write FModifiedColor;
    property PasswordChar:char read FPasswordChar write FPasswordChar;
    property Precision:integer read FPrecision write FPrecision;
    property Prefix:string read FPrefix write FPrefix;
    property Prompt:string read FPrompt write FPrompt;
    property ShowHint:boolean read FShowHint write FShowHint;
    property ShowModified:boolean read FShowModified write FShowModified default false;
    property ShowURL:boolean read FShowURL write FShowURL default false;
    property Signed:boolean read FSigned write FSigned default false;
    property Suffix:string read FSuffix write FSuffix;
    property Text:string read FText write FText;
    property ValidChars:string read FValidChars write FValidChars;
    property Version: string read GetVersion write SetVersion;
  end;


implementation


{ TAdvQueryDialog }

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

function TAdvQueryDialog.ShowModal: TModalResult;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TAdvEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := FCaption;
      ClientWidth := MulDiv(DialogUnitWidth, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;

      if DialogColor <> clNone then
        Form.Color := DialogColor;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := FPrompt;
      end;
      Edit := TAdvEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(DialogUnitWidth - 16, DialogUnits.X, 4);
        MaxLength := 255;
        Text := FText;
        SelectAll;
        CanUndo := FCanUndo;
        EditType := FEditType;
        EditAlign := FEditAlign;
        ExcelStyleDecimalSeparator := FExcelStyleDecimalSeparator; 
        Precision := FPrecision;
        Prefix := FPrefix;
        Suffix := FSuffix;
        Flat := FFlat;
        FocusColor := clNone;
        Color := clWhite;
        FlatLineColor := FFlatLineColor;
        FlatParentColor := FFlatParentColor;
        LengthLimit := FLengthLimit;
        ShowModified := FShowModified;
        ModifiedColor := FModifiedColor;
        ShowURL := FShowURL;
        PasswordChar := FPasswordChar;
        Signed := FSigned;
        Hint := FHint;
        ShowHint := FShowHint;
        ValidChars := FValidChars;
        if Flat then
          Transparent := true;
        DefaultHandling := false;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;

        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv((DialogUnitWidth div 2 - 50) - 5, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv((DialogUnitWidth div 2) + 5, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      
      result := ShowModal;

      if result=mrOk then FText := Edit.Text;

    finally
      Form.Free;
    end;
end;

constructor TAdvQueryDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDialogUnitWidth := 180;
  FDialogColor := clNone;
end;

function TAdvQueryDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvQueryDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvQueryDialog.SetVersion(const Value: string);
begin

end;

end.
