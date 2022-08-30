{**************************************************************************}
{ TAdvUpDownEdit component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2013                                                         }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvUpDownEdit;

interface

uses
  Classes, AdvMultiButtonEdit, AdvEdit;


type
  TUpDownType = (udInteger, udFloat);

  TAdvUpDownEdit = class(TAdvCustomMultiButtonEdit)
  private
    FIncrementFloat: double;
    FIncrement: integer;
    FUpDownType: TUpDownType;
    FPrecision: integer;
    function GetMaxFloatValue: double;
    procedure SetMaxFloatValue(const Value: double);
    procedure SetMaxValue(const Value: integer);
    procedure SetMinValue(const Value: integer);
    function GetMaxValue: integer;
    function GetMinFloatValue: double;
    function GetMinValue: integer;
    procedure SetMinFloatValue(const Value: double);
    function GetPrecision: integer;
    procedure SetPrecision(const Value: integer);
    procedure SetUpDownType(const Value: TUpDownType);
    function GetEditAlign: TEditAlign;
    procedure SetEditAlign(const Value: TEditAlign);
    function UseMinMax: boolean;
    function UseMinMaxFloat: boolean;
    function GetButtonAdd: TEditButton;
    function GetButtonSub: TEditButton;
    function GetEditorEnabled: boolean;
    procedure SetEditorEnabled(const Value: boolean);
  protected
    procedure DoClickAdd; override;
    procedure DoClickSub; override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    property ButtonAdd: TEditButton read GetButtonAdd;
    property ButtonSub: TEditButton read GetButtonSub;
  published
    property EditAlign: TEditAlign read GetEditAlign write SetEditAlign default eaCenter;
    property EditorEnabled: boolean read GetEditorEnabled write SetEditorEnabled default true;
    property MaxFloatValue: double read GetMaxFloatValue write SetMaxFloatValue;
    property MinFloatValue: double read GetMinFloatValue write SetMinFloatValue;
    property MaxValue: integer read GetMaxValue write SetMaxValue default 0;
    property MinValue: integer read GetMinValue write SetMinValue default 0;
    property Increment: integer read FIncrement write FIncrement default 1;
    property IncrementFloat: double read FIncrementFloat write FIncrementFloat;
    property Precision: integer read GetPrecision write SetPrecision default 0;
    property UpdownType: TUpDownType read FUpDownType write SetUpDownType default udInteger;
    property OnClickAdd;
    property OnClickSub;
  end;



implementation

uses
  Math;

{ TAdvUpDownEdit }

constructor TAdvUpDownEdit.Create(AOwner: TComponent);
begin
  inherited;
  Buttons.Clear;
  Buttons.Add;
  Buttons.Add;
  Buttons[0].Position := bpLeft;
  Buttons[0].Style := bsSub;
  Buttons[1].Position := bpRight;
  Buttons[1].Style := bsAdd;
  FIncrement := 1;
  FIncrementFloat := 0.1;
  FUpdownType := udInteger;
  Edit.EditType := etNumeric;
  Edit.EditAlign := eaCenter;
  Edit.Precision := 2;
end;

procedure TAdvUpDownEdit.DoClickAdd;
begin
  inherited;

  if FUpdownType = udFloat then
  begin
    if UseMinMaxFloat then
      Edit.FloatValue := Min(Edit.MaxFloatValue,Edit.FloatValue + IncrementFloat)
    else
      Edit.FloatValue := Edit.FloatValue + IncrementFloat
  end
  else
  begin
    if UseMinMax then
      Edit.IntValue := Min(Edit.MaxValue, Edit.IntValue + Increment)
    else
      Edit.IntValue := Edit.IntValue + Increment;
  end;
  Edit.SelectAll;
end;

procedure TAdvUpDownEdit.DoClickSub;
begin
  inherited;
  if FUpdownType = udFloat then
  begin
    if UseMinMaxFloat then
      Edit.FloatValue := Max(Edit.MinFloatValue,Edit.FloatValue - IncrementFloat)
    else
      Edit.FloatValue := Edit.FloatValue - IncrementFloat
  end
  else
  begin
    if UseMinMax then
      Edit.IntValue := Max(Edit.MinValue,Edit.IntValue - Increment)
    else
      Edit.IntValue := Edit.IntValue - Increment;
  end;

  Edit.SelectAll;
end;

function TAdvUpDownEdit.GetButtonAdd: TEditButton;
begin
  Result := Buttons[1];
end;

function TAdvUpDownEdit.GetButtonSub: TEditButton;
begin
  Result := Buttons[0];
end;

function TAdvUpDownEdit.GetEditAlign: TEditAlign;
begin
  Result := Edit.EditAlign;
end;

function TAdvUpDownEdit.GetEditorEnabled: boolean;
begin
  Result := Edit.EditorEnabled;
end;

function TAdvUpDownEdit.GetMaxFloatValue: double;
begin
  Result := Edit.MaxFloatValue;
end;

function TAdvUpDownEdit.GetMaxValue: integer;
begin
  Result := Edit.MaxValue;
end;

function TAdvUpDownEdit.GetMinFloatValue: double;
begin
  Result := Edit.MinFloatValue;
end;

function TAdvUpDownEdit.GetMinValue: integer;
begin
  Result := Edit.MinValue;
end;

function TAdvUpDownEdit.GetPrecision: integer;
begin
  Result := FPrecision;
end;

procedure TAdvUpDownEdit.SetEditAlign(const Value: TEditAlign);
begin
  Edit.EditAlign := Value;
end;

procedure TAdvUpDownEdit.SetEditorEnabled(const Value: boolean);
begin
  Edit.EditorEnabled := Value;
end;

procedure TAdvUpDownEdit.SetMaxFloatValue(const Value: double);
begin
  Edit.MaxFloatValue := Value;
end;

procedure TAdvUpDownEdit.SetMaxValue(const Value: integer);
begin
  Edit.MaxValue := Value;
  Edit.Signed := Edit.MinValue < 0;
end;

procedure TAdvUpDownEdit.SetMinFloatValue(const Value: double);
begin
  Edit.MinFloatValue := Value;
  Edit.Signed := Edit.MinFloatValue < 0;
end;

procedure TAdvUpDownEdit.SetMinValue(const Value: integer);
begin
  Edit.MinValue := Value;
end;

procedure TAdvUpDownEdit.SetPrecision(const Value: integer);
begin
  FPrecision := Value;
  Edit.Precision := Value;
end;

procedure TAdvUpDownEdit.SetUpDownType(const Value: TUpDownType);
begin
  FUpDownType := Value;
  if FUpDownType = udInteger then
    Edit.EditType := etNumeric;

  if FUpDownType = udFloat then
  begin
    Edit.EditType := etFloat;
    Edit.Precision := FPrecision;
  end;

end;

function TAdvUpDownEdit.UseMinMax: boolean;
begin
  Result := (Edit.MinValue <> 0) or (Edit.MaxValue <> 0);
end;

function TAdvUpDownEdit.UseMinMaxFloat: boolean;
begin
  Result := (Edit.MinFloatValue <> 0) or (Edit.MaxFloatValue <> 0);
end;

end.
