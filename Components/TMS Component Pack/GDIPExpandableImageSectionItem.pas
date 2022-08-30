{*************************************************************************}
{ TExpandableImageSectionItem Class                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2013                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPExpandableImageSectionItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Classes, Controls, Graphics, GDIPImageSectionItem,
  GDIPCustomItem, AdvGDIP, GDIPFill, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TExpanderState = (esNormal, esDown, esHovered);

  TExpandableImageSectionItem = class(TImageSectionItem)
  private
    FExpanderVisible: Boolean;
    FExpanderSize: Integer;
    FExpanderLocation: TItemLocation;
    FExpanded: Boolean;
    FExpanderColor: TColor;
    FExpanderHoverColor: TColor;
    FExpanderDownColor: TColor;
    FExpanderBorderColor: TColor;
    FExpanderState: TExpanderState;
    FOnInternalItemExpand: TItemEvent;
    FOnItemExpand: TItemEvent;
    FExpanderTop: Integer;
    FExpanderLeft: Integer;
    procedure SetExpanderVisible(const Value: Boolean);
    procedure SetExpanderLocation(const Value: TItemLocation);
    procedure SetExpanderSize(const Value: Integer);
    procedure SetExpanded(const Value: Boolean);
    procedure SetExpanderColor(const Value: TColor);
    procedure SetExpanderBorderColor(const Value: TColor);
    procedure SetExpanderDownColor(const Value: TColor);
    procedure SetExpanderHoverColor(const Value: TColor);
    procedure SetExpanderState(const Value: TExpanderState);
    procedure SetExpanderLeft(const Value: Integer);
    procedure SetExpanderTop(const Value: Integer);
  protected
    procedure DrawExpander(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure DoItemExpand(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemExpand(Sender: TObject; Item: TCustomItem); virtual;
    property OnInternalItemExpand: TItemEvent read FOnInternalItemExpand write FOnInternalItemExpand;
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    procedure Assign(Source: TPersistent); override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    function IsExpanderAtXY(pX, pY: Integer): Boolean;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    property ExpanderState: TExpanderState read FExpanderState write SetExpanderState;
    procedure ClearItemState; override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure AssignEvents(Item: TCustomItem); override;
  published
    property ExpanderLeft: Integer read FExpanderLeft write SetExpanderLeft default 0;
    property ExpanderTop: Integer read FExpanderTop write SetExpanderTop default 0;
    property ExpanderVisible: Boolean read FExpanderVisible write SetExpanderVisible default true;
    property ExpanderSize: Integer read FExpanderSize write SetExpanderSize default 14;
    property ExpanderLocation: TItemLocation read FExpanderLocation write SetExpanderLocation default tlCenterRight;
    property Expanded: Boolean read FExpanded write SetExpanded default true;
    property ExpanderColor: TColor read FExpanderColor write SetExpanderColor default clSilver;
    property ExpanderBorderColor: TColor read FExpanderBorderColor write SetExpanderBorderColor default clBlack;
    property ExpanderDownColor: TColor read FExpanderDownColor write SetExpanderDownColor default clGray;
    property ExpanderHoverColor: TColor read FExpanderHoverColor write SetExpanderHoverColor;
    property OnItemExpand: TItemEvent read FOnItemExpand write FOnItemExpand;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TExpandableImageSectionItem);
end;

{ TExpandableImageSectionItem }

procedure TExpandableImageSectionItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TExpandableImageSectionItem then
  begin
    FExpanderVisible := (Source as TExpandableImageSectionItem).ExpanderVisible;
    FExpanderSize := (Source as TExpandableImageSectionItem).ExpanderSize;
    FExpanderLocation := (Source as TExpandableImageSectionItem).ExpanderLocation;
    FExpanderColor := (Source as TExpandableImageSectionItem).ExpanderColor;
    FExpanderBorderColor := (Source as TExpandableImageSectionItem).ExpanderBorderColor;
    FExpanderDownColor := (Source as TExpandableImageSectionItem).ExpanderDownColor;
    FExpanderHoverColor := (Source as TExpandableImageSectionItem).ExpanderHoverColor;
  end;
end;

procedure TExpandableImageSectionItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is TExpandableImageSectionItem then
  begin
    OnItemExpand := (Item as TExpandableImageSectionItem).OnItemExpand;
  end;
end;

procedure TExpandableImageSectionItem.ClearItemState;
begin
  inherited;
  ExpanderState := esNormal;
end;

constructor TExpandableImageSectionItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExpanderVisible := true;
  FExpanderSize := 18;
  FExpanderLocation := tlCenterRight;
  FExpanded := true;
  FExpanderColor := clSilver;
  FExpanderHoverColor := RGB(255, 50, 0);
  FExpanderBorderColor := clBlack;
  FExpanderDownColor := clGray;
  FExpanderLeft := 0;
  FExpanderTop := 0;
end;

function TExpandableImageSectionItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TExpandableImageSectionItem.Create(AOwner);
end;

class function TExpandableImageSectionItem.CustomClassName: String;
begin
  Result := 'Section Expandable Image Item';
end;

destructor TExpandableImageSectionItem.Destroy;
begin
  inherited;
end;

procedure TExpandableImageSectionItem.DoInternalItemExpand(Sender: TObject;
  Item: TCustomItem);
begin
  if Assigned(OnInternalItemExpand) then
    OnInternalItemExpand(Sender, Item);
end;

procedure TExpandableImageSectionItem.DoItemExpand(Sender: TObject;
  Item: TCustomItem);
var
  expandif: IGDIPExpand;
begin
  if Assigned(OnItemExpand) then
    OnItemExpand(Sender, Item)
  else
  begin
    if Assigned(ItemOwner) then
      if ItemOwner.GetInterface(IGDIPExpand, expandif) then
        expandif.Expand(Self, Expanded);
  end;
end;

procedure TExpandableImageSectionItem.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; pX, pY: Integer;
  Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itExpander: ExpanderState := esDown;
    end;
  end;
end;

procedure TExpandableImageSectionItem.DoMouseMove(Sender: TObject;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itExpander: ExpanderState := esHovered;
    end;
  end;
end;

procedure TExpandableImageSectionItem.DoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; pX, pY: Integer;
  Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itExpander:
      begin
        Expanded := not Expanded;
        ExpanderState := esNormal;
      end;
    end;
  end;
end;

procedure TExpandableImageSectionItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    inherited;
    DrawExpander(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r)
  end;
end;

procedure TExpandableImageSectionItem.DrawExpander(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  fl: TGDIPFill;
  p: TGPPen;
  expx, expy: Double;
  r, expr: TGPRectF;
begin
  if ExpanderVisible then
  begin
    fl := TGDIPFill.Create;
    case ExpanderState of
      esNormal: fl.Color := ExpanderColor;
      esDown: fl.Color := ExpanderDownColor;
      esHovered: fl.Color := ExpanderHoverColor;
    end;
    fl.BorderColor := ExpanderBorderColor;
    fl.GradientType := gtSolid;
    fl.Rounding := Round(ExpanderSize / 2);
    fl.RoundingType := rtBoth;

    r := ARect;
    if ExpanderLocation <> tlCustom then
      GetObjectLocation(expx, expy, r, ExpanderSize, ExpanderSize, ExpanderLocation)
    else
    begin
      expx := ExpanderLeft;
      expy := ExpanderTop;
    end;

    expr := MakeRect(expx + x, expy + y, ExpanderSize, ExpanderSize);
    fl.Fill(g, expr);
    p := TGPPen.Create(MakeColor(255, ExpanderBorderColor));
    if not Expanded then
    begin
      g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 3 * 2), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 3));
      g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 3 * 2), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 3));
    end
    else
    begin
      g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 3), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 3 * 2));
      g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 3), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 3 * 2));
    end;
    p.free;
    fl.Free;
  end;
end;

function TExpandableImageSectionItem.GetClassType: TComponentClass;
begin
  Result := TExpandableImageSectionItem;
end;

function TExpandableImageSectionItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsExpanderAtXY(pX, pY) then
    result := itExpander
  else if IsStatusAtXY(pX, pY) then
    result := itStatus;
end;

function TExpandableImageSectionItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TExpandableImageSectionItem.IsExpanderAtXY(pX,
  pY: Integer): Boolean;
var
  expx, expy: Double;
  r, expr: TGPRectF;
begin
  r := MakeRect(x, y, width, height);
  if ExpanderLocation <> tlCustom then
    GetObjectLocation(expx, expy, r, ExpanderSize, ExpanderSize, ExpanderLocation)
  else
  begin
    expx := ExpanderLeft;
    expy := ExpanderTop;
  end;
  expr := MakeRect(expx + x, expy + y, ExpanderSize, ExpanderSize);
  result := PtInGPRect(expr, Point(px, py));
end;

procedure TExpandableImageSectionItem.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> value then
  begin
    FExpanded := Value;
    DoItemExpand(Self, Self);
    DoInternalItemExpand(Self, Self);
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderBorderColor(
  const Value: TColor);
begin
  if FExpanderBorderColor <> value then
  begin
    FExpanderBorderColor := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderColor(
  const Value: TColor);
begin
  if FExpanderColor <> Value then
  begin
    FExpanderColor := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderDownColor(
  const Value: TColor);
begin
  if FExpanderDownColor <> value then
  begin
    FExpanderDownColor := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderHoverColor(
  const Value: TColor);
begin
  if FExpanderHoverColor <> Value then
  begin
    FExpanderHoverColor := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderLeft(const Value: Integer);
begin
  if FExpanderLeft <> Value then
  begin
    FExpanderLeft := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderLocation(
  const Value: TItemLocation);
begin
  if FExpanderLocation <> value then
  begin
    FExpanderLocation := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderSize(
  const Value: Integer);
begin
  if FExpanderSize <> value then
  begin
    FExpanderSize := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderState(
  const Value: TExpanderState);
begin
  if FExpanderState <> value then
  begin
    FExpanderState := Value;
    RefreshObject;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderTop(const Value: Integer);
begin
  if FExpanderTop <> Value then
  begin
    FExpanderTop := Value;
    Changed;
  end;
end;

procedure TExpandableImageSectionItem.SetExpanderVisible(
  const Value: Boolean);
begin
  if FExpanderVisible <> value then
  begin
    FExpanderVisible := Value;
    Changed;
  end;
end;

end.
