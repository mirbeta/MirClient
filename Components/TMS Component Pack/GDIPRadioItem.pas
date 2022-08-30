{*************************************************************************}
{ TRadioItem Class                                                        }
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


unit GDIPRadioItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, GDIPGraphicItem, GDIPCheckItem, Controls, GDIPCustomItem, AdvGDIP,
  ActnList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TRadioItem = class(TCheckItem, IGDIPGlobalCheck)
  private
    FGroupIndex: integer;
    procedure SetGroupIndex(const Value: integer);
  protected
    procedure GlobalCheck(Item: TCustomItem);
    function GetVersionNr: integer; override;
    function ActionIsGroupIndexLinked: Boolean; override;
    procedure ActionSetGroupIndex(Value: Integer); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawGraphic(g: TGPGraphics; R: TRect; ItemAppearance: TItemAppearance; Kind: TGraphicKind; Checked: Boolean); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    class function Display: Boolean; override;
  published
    property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TRadioItem);
end;

{ TRadioItem }

procedure TRadioItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.GroupIndex = 0) then
      Self.GroupIndex := GroupIndex;
  end;
end;

function TRadioItem.ActionIsGroupIndexLinked: Boolean;
begin
  Result := GroupIndex = (Action as TCustomAction).GroupIndex;
end;

procedure TRadioItem.ActionSetGroupIndex(Value: Integer);
begin
  GroupIndex := Value;
end;

procedure TRadioItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TRadioItem then
  begin
    FGroupIndex := (Source as TRadioItem).GroupIndex;
    Changed;
  end;
end;

constructor TRadioItem.Create(AOwner: TComponent);
begin
  inherited;
  FGroupIndex := 0;
end;

function TRadioItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TRadioItem.Create(AOwner);
end;

class function TRadioItem.CustomClassName: String;
begin
  Result := 'Graphic Radio Item';
end;

class function TRadioItem.Display: Boolean;
begin
  Result := true;
end;

procedure TRadioItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GraphicFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    if not Checked then
      Checked := True;
    GraphicState := gsNormal;
    DoAction;
  end
  else
    inherited;
end;

procedure TRadioItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
begin
  if (Interaction.InteractionType = itGraphic) or ((Interaction.InteractionType = itDefault) and CheckFullArea and (Interaction.InteractionItem = Self)) then
  begin
    if Interaction.InteractionItem = Self then
    begin
      if not Checked then
        Checked := True;
      GraphicState := gsNormal;
      DoAction;
    end;
  end
  else
    inherited;
end;

procedure TRadioItem.DrawGraphic(g: TGPGraphics;
  R: TRect; ItemAppearance: TItemAppearance; Kind: TGraphicKind; Checked: Boolean);
begin
  inherited DrawGraphic(g, R, ItemAppearance, gkRadioButton, Checked);
end;

function TRadioItem.GetClassType: TComponentClass;
begin
  Result := TRadioItem;
end;

function TRadioItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TRadioItem.GlobalCheck(Item: TCustomItem);
var
  it: TRadioItem;
begin
  if Item is TRadioItem then
  begin
    it := TRadioItem(Item);
    if it.GroupIndex = GroupIndex then
      Checked := false;
  end;
end;

procedure TRadioItem.SetGroupIndex(const Value: integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    Changed;
  end;
end;

end.
