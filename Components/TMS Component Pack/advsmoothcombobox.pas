{*************************************************************************}
{ TAdvSmoothComboBox component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2010 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit AdvSmoothComboBox;

{$I TMSDEFS.INC}

interface

uses
  Classes, Graphics, Forms, Controls, Windows, GDIPFill, AdvSmoothListBox,
  AdvStyleIf, SysUtils, Messages, ExtCtrls, Math, Types, AdvGDIP;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  //version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Fixed : Issue with font creation in Delphi7
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  //          : Fixed : OnItemSelected event issue with VK_F4 key and after dropdown
  //          : Fixed : Items.SelectedItem Access violation
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.2.1 : Fixed : Issue with Access Violation when Listbox is dropped down
  // v1.0.2.2 : Fixed : Issue with 0 list items
  // v1.0.2.3 : Fixed : Issue with OnItemSelected and keyboard lookup
  // v2.0.0.0 : New : Database aware version of TAdvSmoothComboBox
  // v2.0.0.1 : Fixed : Issue with selecting item
  // v2.0.0.2 : Fixed : Issue with System parameter working area and dropdownheight
  // v2.0.0.3 : Fixed : Issue with Itemclick event called before selecting item
  // v2.0.1.0 : New : Built-in support for Office 2010 colors
  // v2.0.1.1 : Fixed : Issue with selecting item with keyboard
  // v2.0.1.2 : Fixed : Issue with parent
  // v2.0.1.3 : Fixed : Issue with painting selected item
  // v2.0.1.4 : Fixed : Issue with triggering event OnItemDeleteClick
  //          : Fixed : Issue with painting selected item
  // v2.0.1.5 : Fixed : Issue with assigning listbox events
  // v2.0.1.6 : Improved : Closing combobox with Escape key
  //          : Fixed : Issue with filtering and selecting items
  //          : Fixed : Issue with applying style in OnItemSelected event
  // v2.0.1.7 : Fixed : Issue with OnItemChanged event never called.
  // v2.0.1.8 : Fixed : Issue with selecteditem access in OnCloseUp event
  // v2.0.1.9 : Fixed : Issue with assiging items
  // v2.0.2.0 : Cleanup of outputdebugstring calls
  // v2.0.3.0 : New : Empty Text with text location and font property to display text while no item is selected
  //          : Fixed : Issue with DeleteButton displaying in ComboBox
  // v2.0.3.1 : Improved : Performance in DB version
  // v2.0.3.2 : Fixed : Selection issue in combobox for splitter items
  // v2.0.3.3 : Fixed : issues regarding, selection, focus rectangle, interaction
  // v2.0.3.4 : Fixed : Issue with combobox displaying items
  // v2.1.0.0 : New : Windows 8, Office 2013 styles added
  // v2.1.0.1 : Fixed : Issue with drawing the selected item
  // v2.1.0.2 : Fixed : Issue with assignment header and footer
  // v2.1.0.3 : Fixed : Issue with focus rectangle
  //          : Improved : public ButtonState property
  // v2.1.0.4 : Improved : Performance with large number of items
  // v2.1.0.5 : Fixed : Issue with OnHeaderClick being triggered when dropdown is closed
  // v2.1.0.6 : Fixed : Issue with closing dropdown with F4 key combinations
  // v2.2.0.0 : New : Windows 10, Office 2016 styles added
  // v2.2.0.1 : Fixed : Issue with AutoHeight
  // v2.2.0.2 : Improved : ftGraphic support

type
  TAdvSmoothComboBox = class;

  TAdvSmoothComboBoxButtonState = (bsDown, bsUp, bsHover);

  TAdvSmoothListBoxItemCrack = class(TAdvSmoothListBoxItem);

  TAdvSmoothComboBoxButtonAppearance = class(TPersistent)
  private
    FButtonState: TAdvSmoothComboBoxButtonState;
    FButtonWidth: integer;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FFillDown: TGDIPFill;
    FFillHover: TGDIPFill;
    FArrowColor: TColor;
    FArrowColorHover: TColor;
    FArrowColorDown: TColor;
    procedure SetButtonWidth(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetFillDown(const Value: TGDIPFill);
    procedure SetFillHover(const Value: TGDIPFill);
    procedure SetArrowColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothComboBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;
    property ArrowColorHover: TColor read FArrowColorHover write FArrowColorHover default clBlack;
    property ArrowColorDown: TColor read FArrowColorDown write FArrowColorDown default clBlack;
    property Fill: TGDIPFill read FFill write SetFill;
    property FillDown: TGDIPFill read FFillDown write SetFillDown;
    property FillHover: TGDIPFill read FFillHover write SetFillHover;
    property Width: integer read FButtonWidth write SetButtonWidth default 30;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothComboBoxEmptyTextLocation = (etlTopLeft, etlTopCenter, etlTopRight, etlCenterLeft, etlCenterCenter, etlCenterRight, etlBottomLeft, etlBottomCenter, etlBottomRight);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothComboBox = class(TAdvSmoothListBox, ITMSTones)
  private
    FTimerCount: integer;
    FLookupKey: String;
    FFocused, FMouseDown: Boolean;
    FClickY, FClickX: integer;
    FListBoxMouseDown, FListBoxMouseMoved, FDesignTime, FConstructed: Boolean;
    CancelThisBtnClick : Boolean;
    FTimer: TTimer;
    FDeactivating: boolean;
    LstParent : TForm;
    FFill: TGDIPFill;
    FListBox: TAdvSmoothListBox;
    FButtonAppearance: TAdvSmoothComboBoxButtonAppearance;
    FAutoHeight: Boolean;
    FDropDownHeight: integer;
    FDropDownWidth: integer;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FSkipKeyScrolling: Boolean;
    FText: String;
    FTextLocation: TAdvSmoothComboBoxEmptyTextLocation;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSelectedItemIndex(const Value: integer);
    procedure SetButtonAppearance(
      const Value: TAdvSmoothComboBoxButtonAppearance);
    procedure HideParent;
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetDropDownHeight(const Value: integer);
    procedure SetDropDownWidth(const Value: integer);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetText(const Value: String);
    procedure SetTextLocation(const Value: TAdvSmoothComboBoxEmptyTextLocation);
    function GetButtonState: TAdvSmoothComboBoxButtonState;
    procedure SetButtonState(const Value: TAdvSmoothComboBoxButtonState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignCombo(Source: TPersistent);
    procedure Paint; override;
    procedure DropDown; virtual;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure CancelBtnClick;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure SetColorTones(ATones: TColorTones); override;
    property ButtonState: TAdvSmoothComboBoxButtonState read GetButtonState write SetButtonState;
  protected
    FSelectedItemIndex: integer;
    procedure HeaderFooterChanged(Sender: TObject); override;
    procedure SetInternalSelectedItemIndex(Value: Integer); virtual;
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure ButtonAppearanceChanged(Sender: TObject);
    procedure DrawBackGround;
    procedure DrawButton;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure DoButtonClick;
    procedure LstParentDeactivate(Sender: TObject);
    function InsideRect: TRect;
    function GetItemRectangle: TRect;
    function GetButtonRectangle: TRect;
    procedure TimerEvent(Sender: TObject);
    procedure ItemAppearanceChanged(Sender: TObject); override;
    function GetVersionNr: Integer; override;
    procedure DoGraphicMouseDown(X, Y: integer);
    procedure DoGraphicMouseUp(X, Y: integer);
    //Assign listbox events
    procedure ItemChanged(Sender: TObject; itemindex: integer);
    procedure ListBoxItemChanged(Sender: TObject; itemindex: integer);
    procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxItemClick(Sender: TObject; itemindex: integer);
    procedure ListBoxItemSelected(Sender: TObject; itemindex: integer);
    procedure ListBoxItemSelectionChanged(Sender: TObject; previousitemindex, itemindex: integer);
    procedure ListBoxItemChecked(Sender: TObject; itemindex: integer; checked: Boolean);
    procedure ListBoxItemRadioClick(Sender: TObject; itemindex: integer; checked: Boolean);
    procedure ListBoxItemImageClick(Sender: TObject; itemindex: integer);
    procedure ListBoxItemButtonClick(Sender: TObject; itemindex: integer);
    procedure ListBoxItemDeleteClick(Sender: TObject; Item: TAdvSmoothListBoxItem; var Allow: Boolean);
    procedure ListBoxItemCustomizeFont(Sender: TObject; Item: TAdvSmoothListBoxItem; ACaptionFont, ANotesFont, AInfoFont: TFont);
    procedure ListBoxItemCustomizeFill(Sender: TObject; Item: TAdvSmoothListBoxItem; ANormalFill, ADisabledFill, ASelectedFill: TGDIPFill);
    procedure ListBoxScroll(Sender: TObject; CurrentPosition, EndPosition: Double);
    procedure ListBoxShowDetail(Sender: TObject; itemindex: integer);
    procedure ListBoxHideDetail(Sender: TObject; itemindex: integer);
    procedure ListBoxItemText(Sender: TObject; itemindex: integer; var itemcaption: string; var iteminfo: String; var itemnotes: String);
    procedure ListBoxItemBkgDraw(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemRect: TRect; var defaultdraw: Boolean);
    procedure ListBoxItemDraw(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemRect: TRect; var defaultdraw: Boolean);
    procedure ListBoxAnchorClick(Sender: TObject; Anchor: String);
    procedure ListBoxItemAnchorClick(Sender: TObject; Anchor: String; itemindex: integer);
    procedure ListBoxItemCaptionClick(Sender: TObject; itemindex: integer);
    procedure ListBoxItemInfoClick(Sender: TObject; itemindex: integer);
    procedure ListBoxItemMouseLeave(Sender: TObject; itemindex: integer);
    procedure ListBoxItemMouseEnter(Sender: TObject; itemindex: integer);    
    procedure ListBoxLookUpClick(Sender: TObject; lookupindex: integer; lookupvalue: String);
    procedure ListBoxItemHint(Sender: TObject; itemindex: integer; var hint: string);
    procedure ListBoxFooterClick(Sender: TObject; X, Y: integer);
    procedure ListBoxHeaderClick(Sender: TObject; X, Y: integer);
    procedure ListBoxItemDragStart(Sender: TObject; DragItemIndex: integer; var allowdrag: Boolean);
    procedure ListBoxItemDragDrop(Sender: TObject; DragItemIndex, DropItemIndex: integer; var allowdrop: Boolean);
    procedure ListBoxItemDragEnd(Sender: TObject; DragItemIndex: integer);
    procedure ListBoxItemDragOver(Sender: TObject; DragItemIndex, DropItemIndex: integer);
    procedure ListBoxItemDblClick(Sender: TObject; itemindex: integer);
    {$IFDEF DELPHI2006_LVL}
    procedure ListBoxMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; HitTest: Integer; var MouseActivate: TMouseActivate);
    procedure ListboxMouseEnter(Sender: TObject);
    procedure ListboxMouseLeave(Sender: TObject);
    {$ENDIF}
    procedure ListBoxEnter(Sender: TObject);
    procedure ListBoxExit(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListBoxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListBoxResize(Sender: TObject);

    function CreateListBox: TAdvSmoothListBox; virtual;
    property SkipKeyScrolling: Boolean read FSkipKeyScrolling write FSkipKeyScrolling;
    property ListBox: TAdvSmoothListBox read FListBox;
  public
    function YToDeleteButton(X, Y: integer): integer; override;
    function YToItem(X, Y: integer; CountSections: Boolean = false; CheckDisplayRectangle: Boolean = true): integer; override;
  published
    property Font;
    property TextLocation: TAdvSmoothComboBoxEmptyTextLocation read FTextLocation write SetTextLocation default etlCenterLeft;
    property Text: String read FText write SetText;
    property ItemBackGroundFill: TGDIPFill read FFill write SetFill;
    property SelectedItemIndex: integer read FSelectedItemIndex write SetSelectedItemIndex default -1;
    property ButtonAppearance: TAdvSmoothComboBoxButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default true;
    property DropDownHeight: integer read FDropDownHeight write SetDropDownHeight default 350;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth default 250;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;


implementation

procedure GetEmptyTextPosition(var x, y: integer; rectangle: TRect; objectwidth, objectheight: integer; location: TAdvSmoothComboBoxEmptyTextLocation);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Right - rectangle.Left;
  h := rectangle.Bottom - rectangle.Top;
  case location of
    etlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    etlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    etlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    etlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    etlTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    etlBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    etlCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    etlCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    etlCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;
end;


{ TAdvSmoothComboBox }

procedure TAdvSmoothComboBox.Assign(Source: TPersistent);
begin
  inherited;
  AssignCombo(Source);
end;

procedure TAdvSmoothComboBox.AssignCombo(Source: TPersistent);
begin
  if (Source is TAdvSmoothComboBox) then
  begin
    FFill.Assign((Source as TAdvSmoothComboBox).Fill);
    FSelectedItemIndex := (Source as TAdvSmoothComboBox).SelectedItemIndex;
    FButtonAppearance.Assign((Source as TAdvSmoothComboBox).ButtonAppearance);
    FAutoHeight := (Source as TAdvSmoothComboBox).AutoHeight;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.ButtonAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothComboBox.ListBoxAnchorClick(Sender: TObject;
  Anchor: String);
begin
  if Assigned(OnAnchorClick) then
    OnAnchorClick(Sender, Anchor);
end;

procedure TAdvSmoothComboBox.ListBoxClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Sender);
end;

procedure TAdvSmoothComboBox.ListBoxDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Sender);
end;

procedure TAdvSmoothComboBox.ListBoxItemDblClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemDblClick) then
    OnItemDblClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemDeleteClick(Sender: TObject;
  Item: TAdvSmoothListBoxItem; var Allow: Boolean);
begin
  if Assigned(OnItemDeleteClicked) then
    OnItemDeleteClicked(Sender,Item,Allow);
end;

procedure TAdvSmoothComboBox.ListBoxShowDetail(Sender: TObject; itemindex: integer);
begin
  if Assigned(OnShowDetail) then
    OnShowDetail(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxStartDrag(Sender: TObject;
    var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    OnStartDrag(Sender, DragObject);
end;

procedure TAdvSmoothComboBox.ListBoxFooterClick(Sender: TObject; X, Y: integer);
begin
  if Assigned(OnFooterClick) then
    OnFooterClick(Sender, X, Y);
end;

procedure TAdvSmoothComboBox.ListBoxHeaderClick(Sender: TObject; X, Y: integer);
begin
  if Assigned(OnHeaderClick) then
    OnHeaderClick(Sender, X, Y);
end;

procedure TAdvSmoothComboBox.ListBoxHideDetail(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnHideDetail) then
    OnHideDetail(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemAnchorClick(Sender: TObject;
  Anchor: String; itemindex: integer);
begin
  if Assigned(OnItemAnchorClick) then
    OnItemAnchorClick(Sender, Anchor, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemBkgDraw(Sender: TObject;
  Canvas: TCanvas; itemindex: integer; itemRect: TRect;
  var defaultdraw: Boolean);
begin
  if Assigned(OnItemBkgDraw) then
    OnItemBkgDraw(Sender, Canvas, itemindex, itemrect, defaultdraw);
end;

procedure TAdvSmoothComboBox.ListBoxItemButtonClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemButtonClick) then
    OnItemButtonClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemCaptionClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemCaptionClick) then
    OnItemCaptionClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemChanged(Sender: TObject; itemindex: integer);
begin
  if (csLoading in ComponentState) then
    Exit;

  if (itemindex >= 0) and (itemindex < FListBox.Items.Count) and (itemindex < Items.Count) then
  begin
    if not Items[itemindex].Splitter then
    begin
      Items.BeginUpdate;
      Items[itemindex].CopySettings(FListBox.Items[itemindex]);
      Items.EndUpdate;
      Changed;
      if Assigned(OnItemChanged) then
        OnItemChanged(Self, itemindex);
    end;
  end;
end;

procedure TAdvSmoothComboBox.ListBoxItemChecked(Sender: TObject;
  itemindex: integer; checked: Boolean);
begin
  if Assigned(OnItemCheckClick) then
    OnItemCheckClick(Sender, itemindex, checked);
end;

procedure TAdvSmoothComboBox.ListBoxItemClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemCustomizeFill(Sender: TObject; Item: TAdvSmoothListBoxItem;
  ANormalFill, ADisabledFill, ASelectedFill: TGDIPFill);
begin
  if Assigned(OnItemCustomizeFill) then
    OnItemCustomizeFill(Sender, Item, ANormalFill, ADisabledFill, ASelectedFill);
end;

procedure TAdvSmoothComboBox.ListBoxItemCustomizeFont(Sender: TObject; Item: TAdvSmoothListBoxItem;
  ACaptionFont, ANotesFont, AInfoFont: TFont);
begin
  if Assigned(OnItemCustomizeFont) then
    OnItemCustomizeFont(Sender, Item, ACaptionFont, ANotesFont, AInfoFont);
end;

procedure TAdvSmoothComboBox.ListBoxItemDragDrop(Sender: TObject; DragItemIndex,
  DropItemIndex: integer; var allowdrop: Boolean);
begin
  if Assigned(OnItemDragDrop) then
    OnItemDragDrop(Sender, DragItemIndex, DropItemIndex, AllowDrop);
end;

procedure TAdvSmoothComboBox.ListBoxItemDragEnd(Sender: TObject;
  DragItemIndex: integer);
begin
  if Assigned(OnItemDragEnd) then
    OnItemDragEnd(Sender, DragItemIndex);
end;

procedure TAdvSmoothComboBox.ListBoxItemDragOver(Sender: TObject; DragItemIndex,
  DropItemIndex: integer);
begin
  if Assigned(OnItemDragOver) then
    OnItemDragOver(Sender, DragItemIndex, DropItemIndex);
end;

procedure TAdvSmoothComboBox.ListBoxItemDragStart(Sender: TObject;
  DragItemIndex: integer; var allowdrag: Boolean);
begin
  if Assigned(OnItemDragStart) then
    OnItemDragStart(Sender, DragItemIndex, allowdrag);
end;

procedure TAdvSmoothComboBox.ListBoxItemDraw(Sender: TObject; Canvas: TCanvas;
  itemindex: integer; itemRect: TRect; var defaultdraw: Boolean);
begin
  if Assigned(OnItemDraw) then
    OnItemDraw(Sender, Canvas, itemindex, itemRect, defaultdraw);
end;

procedure TAdvSmoothComboBox.ListBoxItemHint(Sender: TObject;
  itemindex: integer; var hint: string);
begin
  if Assigned(OnItemHint) then
    OnItemHint(Sender, itemindex, hint);
end;

procedure TAdvSmoothComboBox.ListBoxItemImageClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemImageClick) then
    OnItemImageClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemInfoClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemInfoClick) then
    OnItemInfoClick(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemMouseEnter(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemMouseEnter) then
    OnItemMouseEnter(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemMouseLeave(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemMouseLeave) then
    OnItemMouseLeave(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemRadioClick(Sender: TObject;
  itemindex: integer; checked: Boolean);
begin
  if Assigned(OnItemRadioClick) then
    OnItemRadioClick(Sender, itemindex, checked);
end;

procedure TAdvSmoothComboBox.ListBoxItemSelected(Sender: TObject;
  itemindex: integer);
begin
  if (itemindex >= 0) and (itemindex < FListBox.Items.Count) then
  begin
    if FListBox.Items[itemindex].Splitter then
      Exit;
  end;

  SelectedItemIndex := itemindex;
  if Assigned(OnItemSelected) then
    OnItemSelected(Sender, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemSelectionChanged(Sender: TObject;
  previousitemindex, itemindex: integer);
begin
  if Assigned(OnItemSelectionChanged) then
    OnItemSelectionChanged(Sender, previousitemindex, itemindex);
end;

procedure TAdvSmoothComboBox.ListBoxItemText(Sender: TObject;
  itemindex: integer; var itemcaption, iteminfo, itemnotes: String);
begin
  if Assigned(OnItemText) then
    OnItemText(Sender, itemindex, itemcaption, iteminfo, itemnotes);
end;

procedure TAdvSmoothComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F4) and (Shift = []) then
  begin
    if LstParent.Visible then
      HideParent
    else
      DoButtonClick;
  end;

  if (Key = VK_ESCAPE) then
  begin
    if LstParent.Visible then
      HideParent;
  end;

  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

procedure TAdvSmoothComboBox.ListBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Sender, Key);
end;

procedure TAdvSmoothComboBox.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    HideParent;

  if Assigned(OnKeyUp) then
    OnKeyUp(Sender, Key, Shift);
end;

procedure TAdvSmoothComboBox.ListBoxLookUpClick(Sender: TObject;
  lookupindex: integer; lookupvalue: String);
begin
  if Assigned(OnLookUpClick) then
    OnLookUpClick(Sender, lookupindex, lookupvalue);
end;

procedure TAdvSmoothComboBox.ListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  itemindex: integer;
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Sender, Button, Shift, X, Y);

  FListBoxMouseMoved := false;
  FListBoxMouseDown := true;
  FClickX := X;
  FClickY := Y;
  itemindex := FListBox.ItemAtXY(X, Y);
  if (itemindex > -1) and (itemindex <= FListBox.Items.Count - 1) and FListBox.CheckSelection(x, Y) then
  begin
    FListBox.SelectedItemIndex := itemindex;
    if itemindex <> -1 then
      Items.SelectedItem := Items[itemindex]
    else
      Items.SelectedItem := nil;

    FListBox.Invalidate;
  end;
end;

{$IFDEF DELPHI2006_LVL}
procedure TAdvSmoothComboBox.ListBoxMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  if Assigned(OnMouseActivate) then
    OnMouseActivate(Sender, Button, Shift, X, Y, HitTest, MouseActivate);
end;

procedure TAdvSmoothComboBox.ListboxMouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Sender);
end;

procedure TAdvSmoothComboBox.ListboxMouseLeave(Sender: TObject);
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Sender);
end;
{$ENDIF}

procedure TAdvSmoothComboBox.ListBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if csDesigning in ComponentState then
    Exit;

  FListBoxMouseMoved := FListBoxMouseDown and ((X + 2 < FClickX) or (X - 2 > FClickX) or (Y + 2 < FClickY) or (Y - 2 > FClickY));

  if Assigned(OnMouseMove) then
    OnMouseMove(Sender, Shift, X, Y);
end;

procedure TAdvSmoothComboBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: integer;
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Sender, Button, Shift, X, Y);

  idx := FListBox.ItemAtXY(X, Y);
  if (idx >= 0) and (idx <= FListBox.Items.Count - 1) then
  begin
    if FListBox.Items[idx].Splitter then
      Exit;
  end;

  if not FListBoxMouseMoved and FListBox.CheckSelection(X, Y) then
  begin
    HideParent;
    FListBoxMouseDown := false;
  end;
end;

procedure TAdvSmoothComboBox.ListBoxResize(Sender: TObject);
begin
  if Assigned(OnResize) then
    OnResize(Sender);
end;

procedure TAdvSmoothComboBox.ListBoxScroll(Sender: TObject; CurrentPosition,
  EndPosition: Double);
begin
  if Assigned(OnScroll) then
    OnScroll(Sender, CurrentPosition, EndPosition);
end;

procedure TAdvSmoothComboBox.LstParentDeactivate(Sender: TObject);
var
  ic : TAdvSmoothListBoxItemChanged;
  sc : TAdvSmoothListBoxItemSelectionChangedEvent;
begin
  FDeactivating := true;

  ic := OnItemChanged;
  sc := OnItemSelectionChanged;
  OnItemChanged := nil;
  OnItemSelectionChanged := nil;

  BeginUpdate;
  Assign(FListBox);
  EndUpdate;

  SelectedItemIndex := FListBox.SelectedItemIndex;

  OnItemChanged := ic;
  OnItemSelectionChanged := sc;


  FButtonAppearance.FButtonState := bsUp;
  (Sender as TForm).Hide;
  Setfocus;
  FFocused := true;  
  FTimer.Enabled := true;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TAdvSmoothComboBox.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

procedure TAdvSmoothComboBox.Changed;
begin
  Invalidate;
end;

procedure TAdvSmoothComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FButtonAppearance.FButtonState := bsUp;
  Changed;
end;

function TAdvSmoothComboBox.CreateListBox: TAdvSmoothListBox;
begin
  Result := TAdvSmoothListBox.Create(Self);
end;

constructor TAdvSmoothComboBox.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  Width := 250;
  Height := 30;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  LstParent := TForm.Create(Self);
  LstParent.BorderStyle := bsNone;

  LstParent.Width := 0;
  LstParent.Height := 0;

  FAutoHeight := true;

  OnItemChanged := ItemChanged;

  //FListBox := TAdvSmoothListBox.Create(Self);
  FListBox := CreateListBox;
  FListBox.OnItemChanged := ListBoxItemChanged;
  FListBox.OnItemSelected := ListBoxItemSelected;
  FListBox.OnMouseMove := ListBoxMouseMove;
  FListBox.OnMouseUp := ListBoxMouseUp;
  FListBox.OnMouseDown := ListBoxMouseDown;
  FListBox.OnItemCustomizeFont := ListBoxItemCustomizeFont;
  FListBox.OnItemCustomizeFill := ListBoxItemCustomizeFill;
  FListBox.OnKeyUp := ListBoxKeyUp;
  FListBox.OnHeaderClick := ListBoxHeaderClick;
  FListBox.OnFooterClick := ListBoxFooterClick;
  FListBox.OnItemDragStart := ListBoxItemDragStart;
  FListBox.OnItemDragEnd := ListBoxItemDragEnd;
  FListBox.OnItemDragOver := ListBoxItemDragOver;
  FListBox.OnItemDragDrop := ListBoxItemDragDrop;
  FListBox.OnItemMouseLeave := ListBoxItemMouseLeave;
  FListBox.OnItemMouseEnter := ListBoxItemMouseEnter;
  FListBox.OnLookUpClick := ListBoxLookUpClick;
  FListBox.OnItemHint := ListBoxItemHint;
  FListBox.OnItemDblClick := ListBoxItemDblClick;
  FListBox.OnItemClick := ListBoxItemClick;
  FListBox.OnItemSelectionChanged := ListBoxItemSelectionChanged;
  FListBox.OnItemCheckClick := ListBoxItemChecked;
  FListBox.OnItemRadioClick := ListBoxItemRadioClick;
  FListBox.OnItemImageClick := ListBoxItemImageClick;
  FListBox.OnItemButtonClick := ListBoxItemButtonClick;
  FListBox.OnItemDeleteClicked := ListBoxItemDeleteClick;

  FListBox.OnScroll := ListBoxScroll;
  FListBox.OnHideDetail := ListBoxHideDetail;
  FListBox.OnShowDetail := ListBoxShowDetail;
  FListBox.OnItemDraw := ListBoxItemDraw;
  FListBox.OnItemBkgDraw := ListBoxItemBkgDraw;
  FListBox.OnItemText := ListBoxItemText;
  FListBox.OnAnchorClick := ListBoxAnchorClick;
  FListBox.OnItemAnchorClick := ListBoxItemAnchorClick;
  FListBox.OnItemCaptionClick := ListBoxItemCaptionClick;
  FListBox.OnItemInfoClick := ListBoxItemInfoClick;
  FListBox.OnKeyDown := ListBoxKeyDown;
  FListBox.OnKeyPress := ListBoxKeyPress;
  {$IFDEF DELPHI2006_LVL}
  FListBox.OnMouseActivate := ListBoxMouseActivate;
  FListBox.OnMouseLeave := ListboxMouseLeave;
  FListBox.OnMouseEnter := ListboxMouseEnter;
  {$ENDIF}
  FListBox.OnResize := ListBoxResize;
  FListBox.OnDblClick := ListBoxDblClick;
  FListBox.OnClick := ListBoxClick;
  FListBox.OnEnter := ListBoxEnter;
  FListBox.OnExit := ListBoxExit;
  FListBox.OnDragDrop := ListBoxDragDrop;
  FListBox.OnDragOver := ListBoxDragOver;
  FListBox.OnStartDrag := ListBoxStartDrag;
  FListBox.OnEndDrag := ListBoxEndDrag;

  FListBox.Parent := LstParent;
  FListBox.SetComponentStyle(tsOffice2003Blue);
  FSelectedItemIndex := -1;
  FButtonAppearance := TAdvSmoothComboBoxButtonAppearance.Create(Self);
  FButtonAppearance.OnChange := ButtonAppearanceChanged;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := 100;
  LstParent.OnDeactivate := LstParentDeactivate;

  FDropDownHeight := 350;
  FDropDownWidth := 250;
  FTextLocation := etlCenterLeft;

  TabStop := true;

  FSkipKeyScrolling := False;
  ComboUse := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvSmoothComboBox.CreateWnd;
begin
  inherited;
  if FAutoHeight and not ((csLoading in ComponentState) or (csReading in ComponentState)) then
    Height := ItemAppearance.Height;

  if FConstructed then
    Exit;

  if FDesignTime then
  begin
    //Default combobox style
//    ItemBackGroundFill.Color := RGB(240, 240, 240);
//    ItemBackGroundFill.GradientType := gtSolid;
//    ItemBackGroundFill.BorderColor := clDkGray;
//    ItemBackGroundFill.ColorMirror := RGB(216, 216, 216);
//    ButtonAppearance.Fill.Assign(ItemBackGroundFill);
    SetComponentStyle(tsOffice2007Luna);
  end;

  FConstructed := true;
end;

destructor TAdvSmoothComboBox.Destroy;
begin
  FFill.Free;
  FListBox.Free;
  LstParent.Free;
  FButtonAppearance.Free;
  FTimer.Free;
  inherited;
end;

procedure TAdvSmoothComboBox.DoButtonClick;
begin
  CancelThisBtnClick := False;
  
  inherited;

  if CancelThisBtnClick then
    Exit;

  if FDeactivating then
  begin
    FDeactivating := false;
    Exit;
  end;

  if Assigned(LstParent) then
  begin
    if LstParent.Visible then
    begin
      FDeactivating := true;
      LstParent.Hide;
      Exit;
    end
    else
      DropDown;
  end
  else
    DropDown;
end;

procedure TAdvSmoothComboBox.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;  
end;

procedure TAdvSmoothComboBox.DoExit;
begin
  inherited;
  FFocused := false;
  ButtonAppearance.FButtonState := bsUp;  
  Changed;
end;

procedure TAdvSmoothComboBox.DoGraphicMouseDown(X, Y: integer);
var
  item: integer;
  AnchorI: string;
begin
 //ITEMS
  item := SelectedItemIndex;
  if (item >= 0) and (item <= Items.Count - 1) then
  begin
    with Items[SelectedItemIndex] do
    begin
      AnchorI := GetAnchorAt(X, Y);

      if IsInfo(X, Y) then
        DoItemInfoClick(Self, Index);

      if IsCaption(X, Y) then
        DoItemCaptionClick(Self, Index);

      if (IsGraphicLeft(X, Y)  or ((x = -1) and (y = -1))) and Enabled then
      begin
        if (x = -1) and (y = -1) then
        begin
          case GraphicLeftType of
            gtButton, gtSmoothButton: ButtonLeftDown := true;
          end;
        end
        else
        begin
          case GraphicLeftType of
            gtButton, gtSmoothButton: ButtonLeftDown := not ButtonLeftDown;
          end;
        end;
        Changed;
      end;

      if (IsGraphicRight(X, Y)  or ((x = -1) and (y = -1))) and Enabled then
      begin
        if (x = -1) and (y = -1) then
        begin
          case GraphicRightType of
            gtButton, gtSmoothButton: ButtonRightDown := true;
          end;
        end
        else
        begin
          case GraphicRightType of
            gtButton, gtSmoothButton: ButtonRightDown := not ButtonRightDown;
          end;
        end;
        Changed;
      end;
    end;
  end;

  if AnchorI <> '' then
  begin
    if item <> -1 then
    begin
      with Items[SelectedItemIndex] do
        DoItemAnchorClick(Self, AnchorI, Index);
    end;
  end;
end;

procedure TAdvSmoothComboBox.DoGraphicMouseUp(X, Y: integer);
var
  item, i: integer;
begin
  item := SelectedItemIndex;
  if (item <> -1) and (item <= Items.Count - 1) then
  begin

    with Items[item] do
    begin
      if (IsGraphicLeft(X, Y) or ((x = -1) and (y = -1))) and Enabled and (GraphicLeftType <> AdvSmoothListBox.gtNone) then
      begin
         case GraphicLeftType of
           gtSmoothButton, gtCommonDetailImage, gtImage, gtCommonImage, gtDetailImage: Expanded := not Expanded;
         end;
         case GraphicLeftType of
           gtCheckBox:
           begin
             Checked := not Checked;
             DoItemCheckClick(Self, Index, Checked);
           end;
           gtRadio:
           begin
             for I := 0 to Items.Count - 1 do
               Items[I].Checked := false;

             Checked := not Checked;
             DoItemRadioClick(Self, Index, Checked);
           end;
           gtButton, gtSmoothButton: DoItemButtonClick(Self, Index);
           gtImage, gtCommonImage: DoItemImageClick(Self, Index, True);
           gtDetailImage, gtCommonDetailImage:
           begin
             DoItemImageClick(Self, Index, True);
           end;
        end
      end
      else if (IsGraphicRight(X, Y)  or ((x = -1) and (y = -1))) and Enabled and (GraphicRightType <> AdvSmoothListBox.gtNone) then
      begin
        case GraphicRightType of
         gtCheckBox:
         begin
           Checked := not Checked;
           DoItemCheckClick(Self, Index, Checked);
         end;
         gtRadio:
         begin
           for I := 0 to Items.Count - 1 do
             Items[I].Checked := false;

           Checked := not Checked;
           DoItemRadioClick(Self, Index, Checked);
         end;
         gtButton, gtSmoothButton: DoItemButtonClick(Self, Index);
         gtImage, gtCommonImage: DoItemImageClick(Self, Index, False);
         gtDetailImage, gtCommonDetailImage:
         begin
           DoItemImageClick(Self, Index, False);
         end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothComboBox.DrawBackGround;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  FFill.Fill(g, MakeRect(0, 0, ClientWidth - 1, ClientHeight - 1));
  g.Free;
end;

procedure DrawArrow(Canvas : TCanvas; ArrowColor: TColor; ArrowSize: integer; origin, target : TGPPointF);
type
  TArrayOfPointF = array of TGPPointF;
var
  quarter: byte;
  fx, px: Double;
  fy, py: double;
  x, y: Double;
  arrowpts: TArrayOfPointF;
  p: TGPPointF;
  ar: TGPPointF;
  h: Integer;
  arx, ary: integer;
  g: TGPGraphics;
  b: TGPSolidBrush;
begin

  arx := ArrowSize;
  ary := ArrowSize div 2;

  SetLength(arrowpts, 4);

  arrowpts[0] := target;

  x := target.x - origin.x;
  y := target.y - origin.y;
  h := round(sqrt(sqr(x) + sqr(y)));

  if h = 0 then
    h := 1;

  // quarter?
  if origin.x < target.x then
  begin
    if origin.y < target.y then
      quarter := 1
    else
      quarter := 3;
  end
  else
  begin
    if origin.y < target.y then
      quarter := 2
    else
      quarter := 4;
  end;

  // calculate the actual P position using the adjustments px and py.
  px := x * arx / h;
  py := y * ary / h;
  case quarter of
    1 :
      begin
        p.x := target.x - px;
        p.y := target.y - py;
        ar.x := target.x - (x * arx / h);
        ar.y := target.y - (y * ary / h);
      end;
    2 :
      begin
        p.x := target.x - px;
        p.y := target.y - py;
        ar.x := target.x - (x * arx / h);
        ar.y := target.y - (y * ary / h);
      end;
    3 :
      begin
        p.x := target.x - px;
        p.y := target.y - py;
        ar.x := target.x - (x * arx / h);
        ar.y := target.y - (y * ary / h);
      end;
    4 :
      begin
        p.x := Target.x - px;
        p.y := Target.y - py;
        ar.x := target.x - (x * arx / h);
        ar.y := target.y - (y * ary / h);
      end;
  end;

  //calculate pts[1] and pts[2] from the P position to give us the back of the arrow.
  fx := y * (arx div 2) / h;
  fy := x * (ary div 2) / h;
  case quarter of
    1 :
      begin
        arrowpts[1].x := p.x - fx;
        arrowpts[1].y := p.y + fy;
        arrowpts[3].x := p.x + fx;
        arrowpts[3].y := p.y - fy;
      end;
    2 :
      begin
        arrowpts[1].x := p.x + fx;
        arrowpts[1].y := p.y - fy;
        arrowpts[3].x := p.x - fx;
        arrowpts[3].y := p.y + fy;
      end;
    3 :
      begin
        arrowpts[1].x := p.x + fx;
        arrowpts[1].y := p.y - fy;
        arrowpts[3].x := p.x - fx;
        arrowpts[3].y := p.y + fy;
      end;
    4 :
      begin
        arrowpts[1].x := p.x + fx;
        arrowpts[1].y := p.y - fy;
        arrowpts[3].x := p.x - fx;
        arrowpts[3].y := p.y + fy;
      end;
  end;

  arrowpts[2] := ar;
  if ArrowColor <> clNone then
    canvas.Brush.color := ArrowColor
  else
    canvas.Brush.Style := bsClear;

  g := TGPGraphics.Create(Canvas.Handle);
  b := TGPSolidBrush.Create(MakeColor(255, ArrowColor));
  g.FillPolygon(b, PGPPointF(arrowpts), Length(arrowpts));
  b.Free;
  g.Free;
end;

procedure TAdvSmoothComboBox.DrawButton;
var
  g: TGPGraphics;
  ap: TGPPointF;
  s: Integer;
  f: TGDIPFill;
  clr: TColor;

begin
  with FButtonAppearance do
  begin
    f := FFill;
    case FButtonState of
      bsDown: f := FFillDown;
      bsHover: f := FFillHover;
    end;
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    f.Fill(g, MakeRect(GetButtonRectangle.Left, GetButtonRectangle.Top, GetButtonRectangle.Right - GetButtonRectangle.Left, GetButtonRectangle.Bottom - GetbuttonRectangle.Top));
    g.Free;
    if f.Picture.Empty then
    begin
      s := 8;
      ap.Y := GetButtonRectangle.Top + ((GetButtonRectangle.Bottom - GetButtonRectangle.Top + (s div 2)) div 2);
      ap.X := GetButtonRectangle.Left + ((GetButtonRectangle.Right - GetButtonRectangle.Left) div 2);


      clr := ArrowColor;
      case FButtonState of
      bsDown: clr := ArrowColorDown;
      bsHover: clr := ArrowColorHover;
      end;
      DrawArrow(Canvas, clr, S, MakePoint(ap.X, ap.Y - (s / 2)), MakePoint(ap.x, ap.Y));
    end;
  end;
end;

procedure TAdvSmoothComboBox.DropDown;
var
  LstPos: TPoint;
  r: TRect;
  ic : TAdvSmoothListBoxItemChanged;
  sc : TAdvSmoothListBoxItemSelectionChangedEvent;


  function GetParentWnd: HWnd;
  var
    Last, P: HWnd;
  begin
    Result := 0;
    if Owner <> nil then
    begin
      P := GetParent((Owner as TWinControl).Handle);
      Last := P;
      while P <> 0 do
      begin
        Last := P;
        P := GetParent(P);
      end;
      Result := Last;
    end;
  end;
begin
  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      LstParent.FormStyle := fsStayOnTop;
  end
  else
    LstParent.FormStyle := fsStayOnTop;

  LstPos.x := 0;
  LstPos.y := Height ;
  LstPos := ClientToScreen(LstPos);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...
 
  ic := OnItemChanged;
  sc := OnItemSelectionChanged;
  OnItemChanged := nil;
  OnItemSelectionChanged := nil;

  FListBox.BeginUpdate;
  FListBox.Assign(Self);
  FListBox.EndUpdate;

  OnItemChanged := ic;
  OnItemSelectionChanged := sc;


  if (LstPos.y + DropDownHeight > r.Bottom) then
    LstPos.Y := LstPos.Y - DropDownHeight - Height + 3;

  if (LstPos.x + DropDownWidth > r.right) then
    LstPos.x := LstPos.x - (DropDownWidth - Width);

  try
    if (FSelectedItemIndex <> -1) and (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= Items.Count - 1) then
      FListBox.Items.Select(FSelectedItemIndex);
  except
    on Exception do
       Text := 'exception';
  end;  

  LstParent.Width := 0;
  LstParent.Height := 0;
  LstParent.Show;

  LstParent.Left := LstPos.x;
  LstParent.Top := LstPos.y;
  FListbox.Width := DropDownWidth;
  FListBox.Height := DropDownHeight; 
  LstParent.Width := FListBox.Width;
  LstParent.Height := FListBox.Height;
  FListBox.InitState;
  FListBox.SelectedItemIndex := SelectedItemIndex;
  if (SelectedItemIndex >= 0) and (SelectedItemIndex <= Items.Count - 1) then
    Items.SelectedItem := Items[SelectedItemIndex]
  else
    Items.SelectedItem := nil;
  Invalidate;

  FListBox.TabStop := true;  
  FListBox.SetFocus;
  FFocused := false;

  if Assigned(FOnDropDown) then
    FOnDropDown(Self);

  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
end;

procedure TAdvSmoothComboBox.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothComboBox.GetButtonRectangle: TRect;
begin
  result := Bounds(InsideRect.Right - ButtonAppearance.Width - 1, InsideRect.Top, ButtonAppearance.Width + 1, InsideRect.Bottom - InsideRect.Top);
end;

function TAdvSmoothComboBox.GetButtonState: TAdvSmoothComboBoxButtonState;
begin
  Result := bsDown;
  if Assigned(FButtonAppearance) then
    Result := FButtonAppearance.FButtonState;
end;

function TAdvSmoothComboBox.GetItemRectangle: TRect;
begin
  result := Bounds(InsideRect.Left, InsideRect.Top, InsideRect.Right - InsideRect.Left - ButtonAppearance.Width, InsideRect.Bottom - InsideRect.Top);
end;

function TAdvSmoothComboBox.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

function TAdvSmoothComboBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothComboBox.HeaderFooterChanged(Sender: TObject);
begin
  inherited;
  FListBox.BeginUpdate;
  FListBox.Footer.Assign(Footer);
  FListBox.Header.Assign(Header);
  FListBox.EndUpdate;
end;

procedure TAdvSmoothComboBox.HideParent;
var
  ic : TAdvSmoothListBoxItemChanged;
  sc : TAdvSmoothListBoxItemSelectionChangedEvent;

begin
  FDeactivating := false;
  FButtonAppearance.FButtonState := bsUp;

  ic := OnItemChanged;
  sc := OnItemSelectionChanged;
  OnItemChanged := nil;
  OnItemSelectionChanged := nil;

  BeginUpdate;
  Assign(FListBox);
  EndUpdate;

  LstParent.Hide;
  try
    SetFocus;
    FFocused := true;
  except
  end;

  OnItemChanged := ic;
  OnItemSelectionChanged := sc;
end;

function TAdvSmoothComboBox.InsideRect: TRect;
var
  bw: integer;
begin
  Result := ClientRect;
  // adapt width & height for GDI+ drawing rect
  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (Fill.BorderColor <> clNone) then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

procedure TAdvSmoothComboBox.ItemAppearanceChanged(Sender: TObject);
begin
  if FAutoHeight and not ((csLoading in ComponentState) or (csReading in ComponentState)) then
    Height := ItemAppearance.Height;
  Changed;
end;

procedure TAdvSmoothComboBox.ItemChanged(Sender: TObject; itemindex: integer);
begin
  if (itemindex >= 0) and (itemindex < FListBox.Items.Count) and (itemindex < Items.Count) then
  begin
    FListBox.Items.BeginUpdate;
    FListBox.Items[itemindex].CopySettings(Items[itemindex]);
    FListBox.Items.EndUpdate;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (key = VK_F4) then
  begin
    if LstParent.Visible then
      HideParent
    else
      DoButtonClick;
  end
  else if (Key = VK_SPACE) then
  begin
    DoGraphicMouseDown(-1, -1);
  end
  else
  begin
    if Assigned(Items.SelectedItem) then
    begin
      FSelectedItemIndex := Items.SelectedItem.Index;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothComboBox.KeyPress(var Key: char);
var
  i: integer;
  flg: boolean;
begin
  inherited;

  if not KeyBoardLookup then
    Exit;

  FTimerCount := 0;

  if (Key >= '0') and (Key <= 'z') then
  begin
    FLookupKey := FLookupKey + key;
  end;

  if Key = #8 then
  begin
    if Length(FLookupKey) > 0 then
      Delete(FLookupKey, Length(FLookupKey), 1);
  end;

  flg := false;

  for i := 0 to Items.Count - 1 do
  begin
    if pos(Uppercase(FLookupKey), Uppercase(Items[i].Caption)) = 1 then
    begin
      SelectedItemIndex := i;
      FLookupKey := '';
      flg := true;
      break;
    end;
  end;

  if not flg then
    FLookupKey := '';
end;

procedure TAdvSmoothComboBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_SPACE then
  begin
    DoGraphicMouseUp(-1, -1);
  end;
end;

procedure TAdvSmoothComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  FMouseDown := true;
  if PtInRect(GetButtonRectangle, Point(X, Y)) then
  begin
    ButtonAppearance.FButtonState := bsDown;
    Changed;
  end
  else
  begin
    DoGraphicMouseDown(X, Y);
  end;
end;

procedure TAdvSmoothComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  item: integer;
  AnchorI: String;
begin
  BlockMouseMovement := True;
  inherited;
  BlockMouseMovement := False;

  if PtInRect(GetButtonRectangle, Point(X, Y)) then
  begin
    if (ButtonAppearance.FButtonState <> bsHover) and not FMouseDown then
    begin
      ButtonAppearance.FButtonState := bsHover;
      Changed;
    end;
  end
  else
  begin
    if ButtonAppearance.FButtonState <> bsUp then
    begin
      ButtonAppearance.FButtonState := bsUp;
      Changed;
    end;

    //ITEM ANCHOR
    item := SelectedItemIndex;
    if (item >= 0) and (item <= Items.Count - 1) then
    begin
      with Items[item] do
        AnchorI := GetAnchorAt(X, Y);
    end;

    if (AnchorI <> '') then
      Cursor := crHandPoint
    else
      Cursor := crArrow;
  end;
end;

procedure TAdvSmoothComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if csDesigning in ComponentState then
    Exit;

  FMouseDown := false;

  inherited;

  if PtInRect(GetButtonRectangle, Point(X, Y)) then
  begin
    ButtonAppearance.FButtonState := bsUp;
    Changed;
    DoButtonClick;
  end
  else
    DoGraphicMouseUp(X, Y);
end;

procedure TAdvSmoothComboBox.Paint;
var
  ir, r: TRect;
  x, y: integer;
  c: Boolean;
  g: TGPGraphics;
  pfocus: TGPPen;
  sel, displindex: Integer;
  it: TAdvSmoothListBoxItem;
  tw, th: integer;
  ss: boolean;
  rp: TRect;

begin
  DrawBackGround;
  c := (FSelectedItemIndex <> -1) and (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= Items.Count - 1);
  ir := GetItemRectangle;
  if c then
  begin
    ir := Bounds(ir.Left, ir.Top + GetPosition, ir.Right - ir.Left, ir.Bottom - ir.Top);
    displindex := GetDisplayIndex(FSelectedItemIndex);
    if (displindex >= 0) and (displindex < DisplayList.Count) then
    begin
      sel := FSelectedItemIndex;
      ss := TAdvSmoothListBoxItemCrack(Items[sel]).FSelected;
      TAdvSmoothListBoxItemCrack(Items[sel]).FSelected := false;
      try
        it := DisplayList.GetItem(GetDisplayIndex(FSelectedItemIndex)).DisplayItem;
        DisplayList.GetItem(GetDisplayIndex(FSelectedItemIndex)).ItemRect := ir;
        if it.DeleteButton and it.DeleteButtonVisible then
          ir.Right := ir.Right - ItemAppearance.DeleteButtonWidth;

        CalculateRects(True);
        sel := FSelectedItemIndex;
        ss := Items[sel].Selected;
        Items[sel].Draw(Canvas, ir, GetDisplayIndex(sel), False, True, False);
      finally
        TAdvSmoothListBoxItemCrack(Items[sel]).FSelected := ss;
      end;
    end;
  end
  else
  begin
    if Text <> '' then
    begin
      Canvas.Font.Assign(Font);
      r := Bounds(5, 5, Width - 10 - ButtonAppearance.Width, Height - 10);
      tw := Canvas.TextWidth(Text);
      th := Canvas.TextHeight(Text);
      Canvas.Brush.Style := bsClear;

      GetEmptyTextPosition(x, y, r, tw+1, th+1, TextLocation);
      Canvas.TextOut(r.Left + x, r.Top + y, Text);
    end;
  end;

  DrawButton;

  if Focused and TabStop and ShowFocus then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeDefault);
    pfocus := TGPPen.Create(MakeColor(255, FocusColor), 1);
    rp := GetItemRectangle;
    pfocus.SetDashStyle(DashStyleDot);
    g.DrawRectangle(pfocus, MakeRect(rp.Left + 1, rp.Top + 1, rp.Left + (rp.Right - rp.Left) - 2, rp.Top + (rp.Bottom - rp.Top) - 2));
    pfocus.Free;
    g.Free;
  end;
end;

procedure TAdvSmoothComboBox.Resize;
begin
  inherited;
  if FAutoHeight and not ((csLoading in ComponentState) or (csReading in ComponentState)) then
    Height := ItemAppearance.Height;
end;

procedure TAdvSmoothComboBox.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> value then
  begin
    FAutoHeight := Value;
    if FAutoHeight and not ((csLoading in ComponentState) or (csReading in ComponentState)) then
      Height := ItemAppearance.Height
    else
      Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetButtonAppearance(
  const Value: TAdvSmoothComboBoxButtonAppearance);
begin
  if FButtonAppearance <> value then
  begin
    FButtonAppearance := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetButtonState(
  const Value: TAdvSmoothComboBoxButtonState);
begin
  if Assigned(FButtonAppearance) then
    FButtonAppearance.FButtonState := Value;
end;

procedure TAdvSmoothComboBox.SetColorTones(ATones: TColorTones);
begin
  inherited;
  ButtonAppearance.Fill.Color := ATones.Background.BrushColor;
  ButtonAppearance.Fill.ColorTo := ATones.Background.BrushColor;
  ButtonAppearance.Fill.ColorMirror := ATones.Background.BrushColor;
  ButtonAppearance.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  ButtonAppearance.Fill.BorderColor := ATones.Background.BorderColor;
  ButtonAppearance.ArrowColor := ATones.Background.TextColor;

  ButtonAppearance.FillHover.Color := ATones.Hover.BrushColor;
  ButtonAppearance.FillHover.ColorTo := ATones.Hover.BrushColor;
  ButtonAppearance.FillHover.ColorMirror := ATones.Hover.BrushColor;
  ButtonAppearance.FillHover.ColorMirrorTo := ATones.Hover.BrushColor;
  ButtonAppearance.FillHover.BorderColor := ATones.Hover.BorderColor;
  ButtonAppearance.ArrowColorHover := ATones.Hover.TextColor;

  ButtonAppearance.FillDown.Color := ATones.Selected.BrushColor;
  ButtonAppearance.FillDown.ColorTo := ATones.Selected.BrushColor;
  ButtonAppearance.FillDown.ColorMirror := ATones.Selected.BrushColor;
  ButtonAppearance.FillDown.ColorMirrorTo := ATones.Selected.BrushColor;
  ButtonAppearance.FillDown.BorderColor := ATones.Selected.BorderColor;
  ButtonAppearance.ArrowColorDown := ATones.Selected.TextColor;

  ItemBackGroundFill.Color := ATones.Background.BrushColor;
  ItemBackGroundFill.ColorTo := ATones.Background.BrushColor;
  ItemBackGroundFill.ColorMirror := ATones.Background.BrushColor;
  ItemBackGroundFill.ColorMirrorTo := ATones.Background.BrushColor;
  ItemBackGroundFill.BorderColor := ATones.Background.BorderColor;
end;

procedure TAdvSmoothComboBox.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  ItemBackGroundFill.Assign(ItemAppearance.Fill);
  ButtonAppearance.Fill.Assign(ItemAppearance.Fill);
  ItemBackGroundFill.BorderColor := Header.Fill.BorderColor;
  ButtonAppearance.Fill.BorderColor := Header.Fill.BorderColor;
  ButtonAppearance.FillDown.Assign(ItemAppearance.FillSelected);
  ButtonAppearance.FillDown.BorderColor := Header.Fill.BorderColor;
  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        ButtonAppearance.FillHover.Color := $D2BDB6;
        ButtonAppearance.FillHover.ColorTo := $D2BDB6;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor := $808080;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        ButtonAppearance.FillHover.Color := $EFD3C6;
        ButtonAppearance.FillHover.ColorTo := $EFD3C6;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  clHighlight;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        ButtonAppearance.FillHover.Color := $EBFDFF;
        ButtonAppearance.FillHover.ColorTo := $ACECFF;
        ButtonAppearance.FillHover.ColorMirror := $59DAFF;
        ButtonAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ButtonAppearance.FillHover.BorderColor :=  $99CEDB;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        ButtonAppearance.FillHover.Color := $FBEDD3;
        ButtonAppearance.FillHover.ColorTo := $FAE9C6;
        ButtonAppearance.FillHover.ColorMirror := $F7DAA2;
        ButtonAppearance.FillHover.ColorMirrorTo := $F5D089;
        ButtonAppearance.FillHover.BorderColor :=  clHighlight;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        ButtonAppearance.FillHover.Color := $FDFBFA;
        ButtonAppearance.FillHover.ColorTo := $FDF3EB;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  clHighlight;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        ButtonAppearance.FillHover.Color := clBtnFace;
        ButtonAppearance.FillHover.ColorTo := clBtnFace;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor := clGray;
      end;
      tsOffice2010Blue:
      begin
        ButtonAppearance.FillHover.Color := RGB(253, 227, 138);
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2010Silver:
      begin
        ButtonAppearance.FillHover.Color := RGB(253, 227, 138);
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2010Black:
      begin
        ButtonAppearance.FillHover.Color := RGB(253, 227, 138);
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsWindows8, tsWindows10:
      begin
        ButtonAppearance.FillHover.Color := $F7EFE8;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $F9CEA4;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2013White:
      begin
        ButtonAppearance.FillHover.Color := $FCF0E4;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $EAB47E;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2013LightGray:
      begin
        ButtonAppearance.FillHover.Color := $FCF0E4;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $EAB47E;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2013Gray:
      begin
        ButtonAppearance.FillHover.Color := $F2E1D5;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $F2E1D5;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2016White:
      begin
        ButtonAppearance.FillHover.Color := $F2E1D5;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $F2E1D5;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2016Gray:
      begin
        ButtonAppearance.FillHover.Color := $F2E1D5;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $F2E1D5;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
      tsOffice2016Black:
      begin
        ButtonAppearance.FillHover.Color := $6A6A6A;
        ButtonAppearance.FillHover.ColorTo := clNone;
        ButtonAppearance.FillHover.ColorMirror := clNone;
        ButtonAppearance.FillHover.ColorMirrorTo := clNone;
        ButtonAppearance.FillHover.BorderColor :=  $6A6A6A;
        ButtonAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
  end;

  ButtonAppearance.FillHover.BorderColor := Header.Fill.BorderColor;


  if Assigned(FListBox) then
    FListBox.SetComponentStyle(AStyle);
end;

procedure TAdvSmoothComboBox.SetDropDownHeight(const Value: integer);
begin
  if FDropDownHeight <> value then
  begin
    FDropDownHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetDropDownWidth(const Value: integer);
begin
  if FDropDownWidth <> value then
  begin
    FDropDownWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothComboBox.SetInternalSelectedItemIndex(Value: Integer);
begin
  FSelectedItemIndex := Value;
end;

procedure TAdvSmoothComboBox.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
end;

procedure TAdvSmoothComboBox.SetSelectedItemIndex(const Value: integer);
begin
  if FSelectedItemIndex <> value then
  begin
    if Value >= 0 then
      Items.SelectedItem := Items[Value]
    else
      Items.SelectedItem := nil;
    FSelectedItemIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.SetTextLocation(
  const Value: TAdvSmoothComboBoxEmptyTextLocation);
begin
  if FTextLocation <> Value then
  begin
    FTextLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBox.TimerEvent(Sender: TObject);
begin
  FDeactivating := false;
  FTimer.Enabled :=false;
end;

procedure TAdvSmoothComboBox.WMSetFocus(var Message: TWMSetFocus);
begin
  SetFocus;
end;

function TAdvSmoothComboBox.YToDeleteButton(X, Y: integer): integer;
var
  r: TRect;
  ir: TRect;
  displindex: integer;
  it: TAdvSmoothListBoxItem;
begin
  Result := -1;
  displindex := GetDisplayIndex(FSelectedItemIndex);
  if (displindex >= 0) and (displindex < DisplayList.Count) then
  begin
    it := DisplayList.GetItem(GetDisplayIndex(FSelectedItemIndex)).DisplayItem;
    ir := GetItemRectangle;
    r := Bounds(ir.Right - ItemAppearance.DeleteButtonWidth, ir.Top + (ir.Bottom - ir.Top - ItemAppearance.DeleteButtonHeight) div 2, ItemAppearance.DeleteButtonWidth, ItemAppearance.DeleteButtonHeight);
    if it.DeleteButton and it.DeleteButtonVisible and PtInRect(r, Point(X, Y)) then
      Result := it.Index;
  end;
end;

function TAdvSmoothComboBox.YToItem(X, Y: integer; CountSections: Boolean = false; CheckDisplayRectangle: Boolean = true): integer;
var
  it: TAdvSmoothListBoxItem;
  ir: TRect;
  displindex: integer;
begin
  Result := -1;

  displindex := GetDisplayIndex(FSelectedItemIndex);
  if (displindex >= 0) and (displindex < DisplayList.Count) then
  begin
    it := DisplayList.GetItem(GetDisplayIndex(FSelectedItemIndex)).DisplayItem;
    ir := GetItemRectangle;

    if PtInRect(ir, Point(X, Y + GetPosition)) then
      Result := it.Index;
  end;
end;

procedure TAdvSmoothComboBox.ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Sender, Source, X, Y);
end;

procedure TAdvSmoothComboBox.ListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TAdvSmoothComboBox.ListBoxEndDrag(Sender, Target: TObject;
    X, Y: Integer);
begin
  if Assigned(OnEndDrag) then
    OnEndDrag(Sender, Target, X, Y);
end;

procedure TAdvSmoothComboBox.ListBoxEnter(Sender: TObject);
begin
  if Assigned(OnEnter) then
    OnEnter(Sender);
end;

procedure TAdvSmoothComboBox.ListBoxExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    OnExit(Sender);
end;

{ TAdvSmoothComboBoxButtonAppearance }

procedure TAdvSmoothComboBoxButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothComboBoxButtonAppearance) then
  begin
    FButtonWidth := (Source as TAdvSmoothComboBoxButtonAppearance).Width;
    FFill.Assign((Source as TAdvSmoothComboBoxButtonAppearance).Fill);
    Changed;
  end;
end;

procedure TAdvSmoothComboBoxButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothComboBoxButtonAppearance.Create(
  AOwner: TAdvSmoothComboBox);
begin
  FButtonWidth := 30;
  FArrowColor := clBlack;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FButtonState := bsUp;
  FFillDown := TGDIPFill.Create;
  FFillDown.OnChange := FillChanged;
  FFillHover := TGDIPFill.Create;
  FFillHover.OnChange := FillChanged;
end;

destructor TAdvSmoothComboBoxButtonAppearance.Destroy;
begin
  FFill.Free;
  FFillDown.Free;
  FFillHover.Free;
  inherited;
end;

procedure TAdvSmoothComboBoxButtonAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothComboBoxButtonAppearance.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBoxButtonAppearance.SetButtonWidth(
  const Value: integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothComboBoxButtonAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothComboBoxButtonAppearance.SetFillDown(
  const Value: TGDIPFill);
begin
  if FFillDown <> value then
  begin
    FFillDown.Assign(Value);
    FillChanged(self);
  end;
end;

procedure TAdvSmoothComboBoxButtonAppearance.SetFillHover(
  const Value: TGDIPFill);
begin
  if FFillHover <> value then
  begin
    FFillHover := Value;
    FillChanged(self);
  end;
end;

end.
