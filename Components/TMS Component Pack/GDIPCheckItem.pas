{*************************************************************************}
{ TCheckItem Class                                                        }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
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

unit GDIPCheckItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Classes, GDIPGraphicItem, Controls, GDIPCustomItem, AdvGDIP, ActnList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TCheckItem = class(TGraphicItem)
  private
    FOnInternalItemCheckChanged: TItemCheckEvent;
    FOnItemCheckChanged: TItemCheckEvent;
    FChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  protected
    procedure DoInternalItemCheckChanged(Sender: TObject; Item: TCustomItem; Checked: Boolean); virtual;
    procedure DoItemCheckChanged(Sender: TObject; Item: TCustomItem; Checked: Boolean); virtual;
    property OnInternalItemCheckChanged: TItemCheckEvent read FOnInternalItemCheckChanged write FOnInternalItemCheckChanged;
    function GetVersionNr: integer; override;
    function ActionIsCheckedLinked: Boolean; override;
    procedure ActionSetChecked(Value: Boolean); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    class function Display: Boolean; override;
    procedure AssignEvents(Item: TCustomItem); override;
    procedure DoCMDialogChar(var Message: TCMDialogChar); override;
  published
    property Checked: Boolean read FChecked write SetChecked default false;
    property OnItemCheckChanged: TItemCheckEvent read FOnItemCheckChanged write FOnItemCheckChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TCheckItem);
end;

{ TCheckItem }

procedure TCheckItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.Checked = True) then
      Self.Checked := Checked;
  end;
end;

function TCheckItem.ActionIsCheckedLinked: Boolean;
begin
  Result := Checked = (Action as TCustomAction).Checked;
end;

procedure TCheckItem.ActionSetChecked(Value: Boolean);
begin
  Checked := Value;
end;

procedure TCheckItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCheckItem then
  begin
    FChecked := (source as TCheckItem).Checked;
  end;
end;

procedure TCheckItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is TCheckItem then
  begin
    OnItemCheckChanged := (Item as TCheckItem).OnItemCheckChanged;
  end;
end;

function TCheckItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TCheckItem.Create(AOwner);
end;

class function TCheckItem.CustomClassName: String;
begin
  Result := 'Graphic Check Item';
end;

class function TCheckItem.Display: Boolean;
begin
  Result := true;
end;

procedure TCheckItem.DoCMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  with Message do
  begin
    if IsAccel(CharCode, Caption) then
    begin
      Checked := not Checked;
      GraphicState := gsNormal;
      DoAction;
      Result := 1;
    end;
  end;
end;

procedure TCheckItem.DoInternalItemCheckChanged(Sender: TObject;
  Item: TCustomItem; Checked: Boolean);
begin
  if Assigned(OnInternalItemCheckChanged) then
    OnInternalItemCheckChanged(Sender, Item, Checked);
end;

procedure TCheckItem.DoItemCheckChanged(Sender: TObject; Item: TCustomItem;
  Checked: Boolean);
begin
  if Assigned(OnItemCheckChanged) then
    OnItemCheckChanged(Sender, Item, Checked);
  GraphicFocused := True;
  ItemFocused := False;
  if Assigned(OnInternalFocus) then
    OnInternalFocus(Sender, Item);
end;

procedure TCheckItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GraphicFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    Checked := not Checked;
    GraphicState := gsNormal;
    DoAction;
  end
  else
    inherited;
end;

procedure TCheckItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if (Interaction.InteractionType = itGraphic) or ((Interaction.InteractionType = itDefault) and CheckFullArea and (Interaction.InteractionItem = Self)) then
  begin
    if Interaction.InteractionItem = Self then
    begin
      Checked := not Checked;
      GraphicState := gsNormal;
      DoAction;
    end;
  end
  else
    inherited;
end;

procedure TCheckItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF);
var
  s: integer;
  ri: TRect;
begin
  inherited;
  s := 16;
  case GraphicPosition of
    gpLeft: ri := Bounds(Round(r.x + ControlMargin.Left), Round(r.y + (r.height - s) / 2), s, s);
    gpRight: ri := Bounds(Round(r.x + r.Width - s - ControlMargin.Right), Round(r.y + (r.height - s) / 2), s, s);
  end;
  DrawGraphic(g, ri, ItemAppearance, gkCheckBox, Checked);
end;

function TCheckItem.DrawText(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
var
  s: Integer;
begin
  s := 16;
  case GraphicPosition of
    gpLeft:
    begin
      ARect.X := ARect.X + ControlMargin.Left + s;
      ARect.Width := Arect.Width - ControlMargin.Left - s;
    end;
    gpRight:
    begin
      ARect.Width := ARect.Width - s - ControlMargin.Right;
    end;
  end;

  Result := inherited DrawText(g, ARect, ItemAppearance, DoText);
end;

function TCheckItem.GetClassType: TComponentClass;
begin
  Result := TCheckItem;
end;

function TCheckItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCheckItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    DoInternalItemCheckChanged(Self, self, FChecked);
    DoItemCheckChanged(Self, Self, FChecked);
    DoGlobalCheckChanged(Self, Self, FChecked);
    Changed;
  end;
end;

end.
