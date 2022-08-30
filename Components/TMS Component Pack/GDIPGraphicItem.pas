{*************************************************************************}
{ TGraphicItem Class                                                      }
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

unit GDIPGraphicItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Controls, Graphics, Classes, GDIPCustomItem, AdvGDIP, AdvXPVS,
  GDIPImageTextItem, StdCtrls, SysUtils, ActnList, Menus, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TGraphicPosition = (gpLeft, gpRight);

  TGraphicKind = (gkCheckBox, gkRadioButton, gkButton);

  TGraphicState = (gsNormal, gsDown, gsHovered);

  TGraphicDrawEvent = procedure(Sender: TObject; g: TGPGraphics; Rect: TGPRectF; Item: TCustomItem;
    Checked: Boolean; State: TGraphicState; Kind: TGraphicKind; Enabled: Boolean; var Default: Boolean) of object;

  TGraphicItem = class;

  TGraphicGlyphs = class(TPersistent)
  private
    FOwner: TGraphicItem;
    FCheckedHoverName: String;
    FUnCheckedHoverName: String;
    FCheckedDisabledIndex: integer;
    FUnCheckedDisabledIndex: integer;
    FCheckedDownName: String;
    FUnCheckedDownName: String;
    FCheckedNormalName: String;
    FUnCheckedNormalName: String;
    FCheckedHoverIndex: integer;
    FUnCheckedHoverIndex: integer;
    FCheckedDisabledName: String;
    FUnCheckedDisabledName: String;
    FCheckedDownIndex: integer;
    FUnCheckedDownIndex: integer;
    FCheckedNormalIndex: integer;
    FUnCheckedNormalIndex: integer;
    procedure SetCheckedDisabledIndex(const Value: integer);
    procedure SetCheckedDisabledName(const Value: String);
    procedure SetCheckedDownIndex(const Value: integer);
    procedure SetCheckedDownName(const Value: String);
    procedure SetCheckedHoverIndex(const Value: integer);
    procedure SetCheckedHoverName(const Value: String);
    procedure SetCheckedNormalIndex(const Value: integer);
    procedure SetCheckedNormalName(const Value: String);
    procedure SetUnCheckedDisabledIndex(const Value: integer);
    procedure SetUnCheckedDisabledName(const Value: String);
    procedure SetUnCheckedDownIndex(const Value: integer);
    procedure SetUnCheckedDownName(const Value: String);
    procedure SetUnCheckedHoverIndex(const Value: integer);
    procedure SetUnCheckedHoverName(const Value: String);
    procedure SetUnCheckedNormalIndex(const Value: integer);
    procedure SetUnCheckedNormalName(const Value: String);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TGraphicItem);
    function GetGraphicOwner: TGraphicItem;
    procedure Assign(Source: TPersistent); override;
  published
    property CheckedDisabledIndex: integer read FCheckedDisabledIndex write SetCheckedDisabledIndex default -1;
    property CheckedDisabledName: String read FCheckedDisabledName write SetCheckedDisabledName;
    property CheckedNormalIndex: integer read FCheckedNormalIndex write SetCheckedNormalIndex default -1;
    property CheckedNormalName: String read FCheckedNormalName write SetCheckedNormalName;
    property CheckedDownIndex: integer read FCheckedDownIndex write SetCheckedDownIndex default -1;
    property CheckedDownName: String read FCheckedDownName write SetCheckedDownName;
    property CheckedHoverIndex: integer read FCheckedHoverIndex write SetCheckedHoverIndex default -1;
    property CheckedHoverName: String read FCheckedHoverName write SetCheckedHoverName;
    property UnCheckedDisabledIndex: integer read FUnCheckedDisabledIndex write SetUnCheckedDisabledIndex default -1;
    property UnCheckedDisabledName: String read FUnCheckedDisabledName write SetUnCheckedDisabledName;
    property UnCheckedNormalIndex: integer read FUnCheckedNormalIndex write SetUnCheckedNormalIndex default -1;
    property UnCheckedNormalName: String read FUnCheckedNormalName write SetUnCheckedNormalName;
    property UnCheckedDownIndex: integer read FUnCheckedDownIndex write SetUnCheckedDownIndex default -1;
    property UnCheckedDownName: String read FUnCheckedDownName write SetUnCheckedDownName;
    property UnCheckedHoverIndex: integer read FUnCheckedHoverIndex write SetUnCheckedHoverIndex default -1;
    property UnCheckedHoverName: String read FUnCheckedHoverName write SetUnCheckedHoverName;
  end;

  TGraphicItem = class(TImageTextItem)
  private
    FGraphicFocused: Boolean;
    FIsWinXP: Boolean;
    FGraphicPosition: TGraphicPosition;
    FGraphicEnabled: Boolean;
    FChecked: Boolean;
    FGraphicState: TGraphicState;
    FInteractionFullArea: Boolean;
    FOnDrawGraphic: TGraphicDrawEvent;
    FGraphicGlyphs: TGraphicGlyphs;
    FCheckFullArea: Boolean;
    procedure SetGraphicEnabled(const Value: Boolean);
    procedure SetGraphicPosition(const Value: TGraphicPosition);
    procedure SetGraphicState(const Value: TGraphicState);
    procedure SetInteractionFullArea(const Value: Boolean);
    procedure SetGraphicGlyphs(const Value: TGraphicGlyphs);
    procedure SetCheckFullArea(const Value: Boolean);
  protected
    function GetVersionNr: integer; override;
    function ActionIsCaptionLinked: Boolean; override;
    function ActionIsEnabledLinked: Boolean; override;
    procedure ActionSetCaption(const Value: string); override;
    procedure ActionSetEnabled(Value: Boolean); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    class function Display: Boolean; override;
    procedure DoItemClick(Sender: TObject; Item: TCustomItem); override;
    procedure DrawGraphic(g: TGPGraphics; R: TRect; ItemAppearance: TItemAppearance; Kind: TGraphicKind; Checked: Boolean); virtual;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    function IsGraphicAtXY(pX, pY: integer): Boolean; virtual;
    property GraphicState: TGraphicState read FGraphicState write SetGraphicState;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
     procedure ClearItemState; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    procedure AssignEvents(Item: TCustomItem); override;
    function ProcessTab(Backwards: Boolean): Boolean; override;
    procedure ResetTab; override;
    property GraphicFocused: Boolean read FGraphicFocused write FGraphicFocused;
  published
    property Action;
    property GraphicEnabled: Boolean read FGraphicEnabled write SetGraphicEnabled default true;
    property GraphicPosition: TGraphicPosition read FGraphicPosition write SetGraphicPosition default gpLeft;
    property InteractionFullArea: Boolean read FInteractionFullArea write SetInteractionFullArea default true;
    property CheckFullArea: Boolean read FCheckFullArea write SetCheckFullArea default false;
    property GraphicGlyphs: TGraphicGlyphs read FGraphicGlyphs write SetGraphicGlyphs;
    property OnDrawGraphic: TGraphicDrawEvent read FOnDrawGraphic write FOnDrawGraphic;
  end;

implementation

{ TGraphicItem }

procedure TGraphicItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.Caption = '') then
    begin
      if not UseButtonCaptionForAction then
        Self.Caption := Caption;
    end;
    if not CheckDefaults or (Self.GraphicEnabled = True) then
      Self.GraphicEnabled := Enabled;
  end;
end;

function TGraphicItem.ActionIsCaptionLinked: Boolean;
begin
  Result := AnsiSameCaption(Caption, (Action as TCustomAction).Caption);
end;

function TGraphicItem.ActionIsEnabledLinked: Boolean;
begin
  Result := GraphicEnabled = (Action as TCustomAction).Enabled;
end;

procedure TGraphicItem.ActionSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TGraphicItem.ActionSetEnabled(Value: Boolean);
begin
  GraphicEnabled := Value;
end;

procedure TGraphicItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGraphicItem then
  begin
    FGraphicPosition := (Source as TGraphicItem).GraphicPosition;
    FGraphicEnabled := (Source as TGraphicItem).GraphicEnabled;
    FGraphicState := (Source as TGraphicItem).GraphicState;
    FInteractionFullArea := (Source as TGraphicItem).InteractionFullArea;
    FCheckFullArea := (Source as TGraphicItem).CheckFullArea;
    FGraphicGlyphs.Assign(FGraphicGlyphs);
  end;
end;

procedure TGraphicItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is TGraphicItem then
  begin
    OnDrawGraphic := (Item as TGraphicItem).OnDrawGraphic;
  end;
end;

procedure TGraphicItem.ClearItemState;
begin
  inherited;
  GraphicState := gsNormal;
end;

constructor TGraphicItem.Create(AOwner: TComponent);
{$IFDEF DELPHI7_LVL}
var
  i: integer;
{$ENDIF}
begin
  inherited Create(AOwner);
  FGraphicPosition := gpLeft;
  FGraphicEnabled := True;
  FChecked := false;
  FInteractionFullArea := true;
  FCheckFullArea := false;
  FGraphicGlyphs := TGraphicGlyphs.Create(Self);

  {$IFDEF DELPHI7_LVL}
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsWinXP := (i > 5);
  {$ELSE}
  FIsWinXP := false;
  {$ENDIF}
end;

function TGraphicItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TGraphicItem.Create(AOwner);
end;

class function TGraphicItem.CustomClassName: String;
begin
  Result := 'Graphic Item';
end;

destructor TGraphicItem.Destroy;
begin
  FGraphicGlyphs.Free;
  inherited;
end;

class function TGraphicItem.Display: Boolean;
begin
  Result := false;
end;

procedure TGraphicItem.DoItemClick(Sender: TObject; Item: TCustomItem);
begin
  inherited;
  GraphicFocused := False;
end;

procedure TGraphicItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GraphicFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    GraphicState := gsDown;
  end
  else
    inherited;
end;

procedure TGraphicItem.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionType = itGraphic then
  begin
    if Interaction.InteractionItem = Self then
    begin
      GraphicState := gsDown;
    end;
  end
  else
    inherited;
end;

procedure TGraphicItem.DoMouseMove(Sender: TObject; Shift: TShiftState; pX,
  pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if (Interaction.InteractionType = itGraphic) and InteractionFullArea then
  begin
    Interaction.InteractionType := itDefault;
    inherited DoMouseMove(Sender, Shift, pX, pY, Interaction, ItemAppearance);
    Interaction.InteractionType := itGraphic;
  end
  else
    inherited;

  if Interaction.InteractionType = itGraphic then
  begin
    if Interaction.InteractionItem = Self then
    begin
      if GraphicState <> gsHovered then
        Application.CancelHint;

      if not InteractionFullArea then
        ClearItemState;


      GraphicState := gsHovered;
    end;
  end;
end;

procedure TGraphicItem.DrawGraphic(g: TGPGraphics;
  R: TRect; ItemAppearance: TItemAppearance; Kind: TGraphicKind; Checked: Boolean);
var
  HTheme: THandle;
  DChecked: integer;
  ThemeStyle: DWORD;
  hdl: THandle;
  default: Boolean;
  imgn: String;
  imgi: integer;
  ir: TGPRectF;
  imgl, imgc: Boolean;
begin
  default := true;

  ir := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);

  if Assigned(OnDrawGraphic) then
    OnDrawGraphic(Self, g, ir, Self, Checked, GraphicState, Kind, GraphicEnabled, default);

  if default then
  begin
    DChecked := 0;
    ThemeStyle := 0;
    case Kind of
      gkCheckBox: DChecked := DFCS_BUTTONCHECK;
      gkRadioButton: DChecked := DFCS_BUTTONRADIO;
      gkButton: DChecked := DFCS_BUTTONPUSH;
    end;

    if not GraphicEnabled then
      DChecked := DChecked or DFCS_INACTIVE;

    case GraphicState of
      gsDown: DChecked := DChecked or DFCS_PUSHED;
      gsHovered: DChecked := DChecked or DFCS_HOT;
    end;

    if Checked then
      DChecked := DChecked or DFCS_CHECKED;

    if GraphicEnabled then
    begin
      if Checked then
      begin
        case GraphicState of
          gsNormal: ThemeStyle := CBS_CHECKEDNORMAL;
          gsDown: ThemeStyle := CBS_CHECKEDPRESSED;
          gsHovered: ThemeStyle := CBS_CHECKEDHOT;
        end;
      end
      else
      begin
        case GraphicState of
          gsNormal: ThemeStyle := CBS_UNCHECKEDNORMAL;
          gsDown: ThemeStyle := CBS_UNCHECKEDPRESSED;
          gsHovered: ThemeStyle := CBS_UNCHECKEDHOT;
        end;
      end;
    end
    else
    begin
      if Checked then
        ThemeStyle := CBS_CHECKEDDISABLED
      else
        ThemeStyle := CBS_UNCHECKEDDISABLED;
    end;

    imgi := -1;
    imgn := '';

    if GraphicEnabled then
    begin
      if Checked then
      begin
        case GraphicState of
          gsNormal:
          begin
            imgn := GraphicGlyphs.CheckedNormalName;
            imgi := GraphicGlyphs.CheckedNormalIndex;
          end;
          gsDown:
          begin
            imgn := GraphicGlyphs.CheckedDownName;
            imgi := GraphicGlyphs.CheckedDownIndex;
          end;
          gsHovered:
          begin
            imgn := GraphicGlyphs.CheckedHoverName;
            imgi := GraphicGlyphs.CheckedHoverIndex;
          end;
        end;
      end
      else
      begin
        case GraphicState of
          gsNormal:
          begin
            imgn := GraphicGlyphs.UnCheckedNormalName;
            imgi := GraphicGlyphs.UnCheckedNormalIndex;
          end;
          gsDown:
          begin
            imgn := GraphicGlyphs.UnCheckedDownName;
            imgi := GraphicGlyphs.UnCheckedDownIndex;
          end;
          gsHovered:
          begin
            imgn := GraphicGlyphs.UnCheckedHoverName;
            imgi := GraphicGlyphs.UnCheckedHoverIndex;
          end;
        end;
      end;
    end
    else
    begin
      if Checked then
      begin
        imgn := GraphicGlyphs.CheckedDisabledName;
        imgi := GraphicGlyphs.CheckedDisabledIndex;
      end
      else
      begin
        imgn := GraphicGlyphs.UnCheckedDisabledName;
        imgi := GraphicGlyphs.UnCheckedDisabledIndex;
      end;
    end;

    imgl := DrawImageFromImageList(g, ir, ItemAppearance, imgi);
    imgc := DrawImageFromContainer(g, ir, ItemAppearance, imgn);

    if not imgl and not imgc then
    begin
      hdl := g.GetHDC;

      if FIsWinXP and IsThemeActive then
      begin
        htheme := OpenThemeData(0,'button');
        case Kind of
          gkCheckBox: DrawThemeBackground(HTheme,hdl, BP_CHECKBOX,ThemeStyle,@r,nil);
          gkRadioButton: DrawThemeBackground(HTheme,hdl, BP_RADIOBUTTON,ThemeStyle,@r,nil);
          gkButton: DrawThemeBackground(HTheme, hdl, BP_PUSHBUTTON, ThemeStyle, @r, nil);
        end;
        CloseThemeData(htheme);

      end
      else
        DrawFrameControl(hdl,r,DFC_BUTTON, DChecked);
      g.ReleaseHDC(hdl);
    end;
  end;

  if FGraphicFocused and ItemAppearance.Focus and (ItemAppearance.FocusedItem = Index) then
  begin
    hdl := g.GetHDC;
    InflateRect(r, -2, -2);
    DrawFocusRect(hdl, r);
    g.ReleaseHDC(hdl);
  end;
end;

procedure TGraphicItem.DrawImage(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
begin
  case GraphicPosition of
    gpLeft:
    begin
      ARect.X := ARect.X + ControlMargin.Left + 16;
      ARect.Width := Arect.Width - ControlMargin.Left - 16;
    end;
    gpRight:
    begin
      ARect.Width := ARect.Width - 16 - ControlMargin.Right;
    end;
  end;

  inherited DrawImage(g, ARect, ItemAppearance);
end;

function TGraphicItem.GetClassType: TComponentClass;
begin
  Result := TGraphicItem;
end;

function TGraphicItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsStatusAtXY(pX, pY) then
    result := itStatus
  else if IsGraphicAtXY(pX, pY) then
    result := itGraphic
  else if IsItemAtXY(pX, pY) then
    result := itDefault;
end;

function TGraphicItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TGraphicItem.IsGraphicAtXY(pX, pY: integer): Boolean;
var
  s: Integer;
begin
  //default graphic size
  s := 16;
  Result := false;
  case GraphicPosition of
    gpLeft: Result := PtInRect(Bounds(Round(x + ControlMargin.Left), Round(y + (height - s) / 2), s, s), Point(pX, pY));
    gpRight: Result := PtInRect(Bounds(Round(x + Width - s - ControlMargin.Right), Round(y + (height - s) / 2), s, s), Point(pX, pY));
  end;

  Result := Result and GraphicEnabled;
end;

function TGraphicItem.ProcessTab(Backwards: Boolean): Boolean;
begin
  Result := False;
  if GraphicEnabled then
  begin
    if ItemFocused then
    begin
      if not Backwards then
      begin
        FGraphicFocused := True;
        ItemFocused := False;
        Changed;
      end
      else
      begin
        Result := True;
        Exit;
      end;
    end
    else if FGraphicFocused then
    begin
      if not Backwards then
      begin
        Result := True;
        Exit;
      end
      else
      begin
        FGraphicFocused := False;
        ItemFocused := True;
        Changed;
      end;
    end;
  end
  else
  begin
    Result := True;
  end;
end;

procedure TGraphicItem.ResetTab;
begin
  inherited;
  FGraphicFocused := False;
end;

procedure TGraphicItem.SetCheckFullArea(const Value: Boolean);
begin
  if FCheckFullArea <> Value then
  begin
    FCheckFullArea := Value;
    Changed;
  end;
end;

procedure TGraphicItem.SetGraphicEnabled(const Value: Boolean);
begin
  if FGraphicEnabled <> Value then
  begin
    FGraphicEnabled := Value;
    Changed;
  end;
end;
procedure TGraphicItem.SetGraphicGlyphs(const Value: TGraphicGlyphs);
begin
  if FGraphicGlyphs <> Value then
  begin
    FGraphicGlyphs.Assign(Value);
    Changed;
  end;
end;

procedure TGraphicItem.SetGraphicPosition(const Value: TGraphicPosition);
begin
  if FGraphicPosition <> Value then
  begin
    FGraphicPosition := Value;
    Changed;
  end;
end;

procedure TGraphicItem.SetGraphicState(const Value: TGraphicState);
begin
  if FGraphicState <> Value then
  begin
    FGraphicState := Value;
    RefreshObject;
  end;
end;

procedure TGraphicItem.SetInteractionFullArea(const Value: Boolean);
begin
  if FInteractionFullArea <> Value then
  begin
    FInteractionFullArea := Value;
    Changed;
  end;
end;

{ TGraphicGlyphs }

procedure TGraphicGlyphs.Assign(Source: TPersistent);
begin
  if Source is TGraphicGlyphs then
  begin
    FCheckedDisabledIndex := (Source as TGraphicGlyphs).CheckedDisabledIndex;
    FCheckedDisabledName := (Source as TGraphicGlyphs).CheckedDisabledName;
    FCheckedNormalIndex := (Source as TGraphicGlyphs).CheckedNormalIndex;
    FCheckedNormalName := (Source as TGraphicGlyphs).CheckedNormalName;
    FCheckedDownIndex := (Source as TGraphicGlyphs).CheckedDownIndex;
    FCheckedDownName := (Source as TGraphicGlyphs).CheckedDownName;
    FCheckedHoverIndex := (Source as TGraphicGlyphs).CheckedHoverIndex;
    FCheckedHoverName := (Source as TGraphicGlyphs).CheckedHoverName;
    FUnCheckedDisabledIndex := (Source as TGraphicGlyphs).UnCheckedDisabledIndex;
    FUnCheckedDisabledName := (Source as TGraphicGlyphs).UnCheckedDisabledName;
    FUnCheckedNormalIndex := (Source as TGraphicGlyphs).UnCheckedNormalIndex;
    FUnCheckedNormalName := (Source as TGraphicGlyphs).UnCheckedNormalName;
    FUnCheckedDownIndex := (Source as TGraphicGlyphs).UnCheckedDownIndex;
    FUnCheckedDownName := (Source as TGraphicGlyphs).UnCheckedDownName;
    FUnCheckedHoverIndex := (Source as TGraphicGlyphs).UnCheckedHoverIndex;
    FUnCheckedHoverName := (Source as TGraphicGlyphs).UnCheckedHoverName;
  end;
end;

procedure TGraphicGlyphs.Changed;
begin
  FOwner.Changed;
end;

constructor TGraphicGlyphs.Create(AOwner: TGraphicItem);
begin
  FOwner := AOwner;
  FCheckedDisabledIndex := -1;
  FUnCheckedDisabledIndex := -1;
  FCheckedHoverIndex := -1;
  FUnCheckedHoverIndex := -1;
  FCheckedDownIndex := -1;
  FUnCheckedDownIndex := -1;
  FCheckedNormalIndex := -1;
  FUnCheckedNormalIndex := -1;
end;

function TGraphicGlyphs.GetGraphicOwner: TGraphicItem;
begin
  Result := FOwner;
end;

procedure TGraphicGlyphs.SetCheckedDisabledIndex(const Value: integer);
begin
  if FCheckedDisabledIndex <> Value then
  begin
    FCheckedDisabledIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedDisabledName(const Value: String);
begin
  if FCheckedDisabledName <> Value then
  begin
    FCheckedDisabledName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedDownIndex(const Value: integer);
begin
  if FCheckedDownIndex <> Value then
  begin
    FCheckedDownIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedDownName(const Value: String);
begin
  if FCheckedDownName <> Value then
  begin
    FCheckedDownName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedHoverIndex(const Value: integer);
begin
  if FCheckedHoverIndex <> Value then
  begin
    FCheckedHoverIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedHoverName(const Value: String);
begin
  if FCheckedHoverName <> Value then
  begin
    FCheckedHoverName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedNormalIndex(const Value: integer);
begin
  if FCheckedNormalIndex <> Value then
  begin
    FCheckedNormalIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetCheckedNormalName(const Value: String);
begin
  if FCheckedNormalName <> Value then
  begin
    FCheckedNormalName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedDisabledIndex(const Value: integer);
begin
  if FUnCheckedDisabledIndex <> Value then
  begin
    FUnCheckedDisabledIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedDisabledName(const Value: String);
begin
  if FUnCheckedDisabledName <> Value then
  begin
    FUnCheckedDisabledName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedDownIndex(const Value: integer);
begin
  if FUnCheckedDownIndex <> Value then
  begin
    FUnCheckedDownIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedDownName(const Value: String);
begin
  if FUnCheckedDownName <> Value then
  begin
    FUnCheckedDownName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedHoverIndex(const Value: integer);
begin
  if FUnCheckedHoverIndex <> Value then
  begin
    FUnCheckedHoverIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedHoverName(const Value: String);
begin
  if FUnCheckedHoverName <> Value then
  begin
    FUnCheckedHoverName := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedNormalIndex(const Value: integer);
begin
  if FUnCheckedNormalIndex <> Value then
  begin
    FUnCheckedNormalIndex := Value;
    Changed;
  end;
end;

procedure TGraphicGlyphs.SetUnCheckedNormalName(const Value: String);
begin
  if FUnCheckedNormalName <> Value then
  begin
    FUnCheckedNormalName := Value;
    Changed;
  end;
end;

end.
