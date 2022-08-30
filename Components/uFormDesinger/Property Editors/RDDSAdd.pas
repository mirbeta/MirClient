{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit RDDSAdd;

interface

{$IFDEF MSWINDOWS}
uses SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, Buttons;
{$ENDIF}

{$IFDEF LINUX}
uses SysUtils, Windows, Messages, Classes, QGraphics, QControls,
  QForms, QStdCtrls, QButtons;
{$ENDIF}

type
  TAddFields = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    FieldsList: TListBox;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  public
    procedure SelectAll;
  end;

var
  AddFields: TAddFields;

implementation



{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}

uses LibHelp;

procedure TAddFields.SelectAll;
var
  I: Integer;
begin
  with FieldsList do
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

procedure TAddFields.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
   HelpContext := hcDDataSetAdd;
{$ENDIF}
end;

procedure TAddFields.HelpBtnClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  Application.HelpContext(HelpContext);
{$ENDIF}
end;

end.
