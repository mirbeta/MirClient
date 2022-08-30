unit AdvCardListEditLinks;

interface

uses
  Classes, Spin, Controls, AdvCardList;

type
  { TAdvCardListSpinEditLink }

  TAdvCardListSpinEditLink = class(TCardListEditLink)
  private
    FSpinEdit: TSpinEdit;
    FMaxValue: integer;
    FMinValue: integer;
    FEditorEnabled: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function CreateControl: TWinControl; override;
    procedure SetProperties; override;
    procedure SetSelection(SelStart, SelLength: integer); override;
    procedure SetFocus; override;
    procedure ValueToControl(value: variant); override;
    function ControlToValue: variant; override;
  published
    property EditorEnabled: boolean read FEditorEnabled write FEditorEnabled default true;
    property MinValue: integer read FMinValue write FMinValue default 0;
    property MaxValue: integer read FMaxValue write FMaxValue default 100;
  end;



procedure Register;  

implementation

procedure Register;
begin
  RegisterComponents('TMS CardList', [TAdvCardListSpinEditLink]);
end;

{ TCardListSpinEditLink }

function TAdvCardListSpinEditLink.ControlToValue: variant;
begin
  Result := FSpinEdit.Value;
end;

constructor TAdvCardListSpinEditLink.Create(AOwner: TComponent);
begin
  inherited;
  FMinValue := 0;
  FMaxValue := 100;
  FEditorEnabled := true;
end;

function TAdvCardListSpinEditLink.CreateControl: TWinControl;
begin
  FSpinEdit := TSpinEdit.Create(nil);
  Result := FSpinEdit;
end;

procedure TAdvCardListSpinEditLink.SetFocus;
begin
  FSpinEdit.SetFocus;
end;

procedure TAdvCardListSpinEditLink.SetProperties;
begin
  inherited;
  FSpinEdit.OnKeyDown := ControlKeyDown;
  FSpinEdit.MaxValue := FMaxValue;
  FSpinEdit.MinValue := FMinValue;
  FSpinEdit.EditorEnabled := true;
end;

procedure TAdvCardListSpinEditLink.SetSelection(SelStart,
  SelLength: integer);
begin
  FSpinEdit.SelStart := SelStart;
  FSpinEdit.SelLength := SelLength;
end;

procedure TAdvCardListSpinEditLink.ValueToControl(value: variant);
var
  str: string;
  i,e: integer;
begin
  str := value;

  val(str,i,e);
  if (e = 0) then
    FSpinEdit.Value := i
  else
    FSpinEdit.Value := 0;
end;


end.
