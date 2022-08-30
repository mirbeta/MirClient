unit AdvCardListAdvEditLink;

interface

uses
  Classes, Spin, Controls, AdvCardList, AdvEdit, Forms, Graphics;

type
  { TAdvCardListSpinEditLink }

  TAdvCardListAdvEditLink = class(TCardListEditLink)
  private
    FAdvEdit: TAdvEdit;
    FEditType: TAdvEditType;
    FShowModified: Boolean;
    FModifiedColor: TColor;
    FMaxLength: Integer;
    FPrecision: Integer;
    FPrefix, FSuffix: string;
    FLookup: TLookupSettings;
    procedure SetLookup(const Value: TLookupSettings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateControl: TWinControl; override;
    procedure SetProperties; override;
    procedure SetSelection(SelStart, SelLength: integer); override;
    procedure SetFocus; override;
    procedure ValueToControl(value: variant); override;
    function ControlToValue: variant; override;
  published
    property EditType: TAdvEditType read FEditType write FEditType;
    property Lookup: TLookupSettings read FLookup write SetLookup;
    property MaxLength: Integer read FMaxLength write FMaxLength default 0;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor default clRed;
    property Precision: Integer read FPrecision write FPrecision default 0;
    property Prefix: string read FPrefix write FPrefix;
    property ShowModified: Boolean read FShowModified write FShowModified default false;
    property Suffix: string read FSuffix write FSuffix;
  end;



procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS CardList', [TAdvCardListAdvEditLink]);
end;

{ TCardListSpinEditLink }

function TAdvCardListAdvEditLink.ControlToValue: variant;
begin
  Result := FAdvEdit.Text;
end;

constructor TAdvCardListAdvEditLink.Create(AOwner: TComponent);
begin
  inherited;
  FShowModified := false;
  FModifiedColor := clRed;
  FMaxLength := 0;
  FLookup := TLookupSettings.Create;
end;

function TAdvCardListAdvEditLink.CreateControl: TWinControl;
begin
  FAdvEdit := TAdvEdit.Create(nil);
  Result := FAdvEdit;
end;

destructor TAdvCardListAdvEditLink.Destroy;
begin
  FLookup.Free;
  inherited;
end;

procedure TAdvCardListAdvEditLink.SetFocus;
begin
  FAdvEdit.SetFocus;
end;

procedure TAdvCardListAdvEditLink.SetLookup(const Value: TLookupSettings);
begin
  FLookup.Assign(Value);
end;

procedure TAdvCardListAdvEditLink.SetProperties;
begin
  inherited;
  FAdvEdit.OnKeyDown := ControlKeyDown;
  FAdvEdit.EditType := FEditType;
  FAdvEdit.BorderStyle := bsNone;
  FAdvEdit.ShowModified := FShowModified;
  FAdvEdit.ModifiedColor := FModifiedColor;
  FAdvEdit.MaxLength := FMaxLength;
  FAdvEdit.Prefix := FPrefix;
  FAdvEdit.Suffix := FSuffix;
  FAdvEdit.Precision := FPrecision;
  FAdvEdit.Lookup.Assign(FLookup); 
end;

procedure TAdvCardListAdvEditLink.SetSelection(SelStart,
  SelLength: integer);
begin
  FAdvEdit.SelStart := SelStart;
  FAdvEdit.SelLength := SelLength;
end;

procedure TAdvCardListAdvEditLink.ValueToControl(value: variant);
begin
  FAdvEdit.Text := value;
end;

end.
