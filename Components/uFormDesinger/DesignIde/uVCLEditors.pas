{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{*******************************************************}
{         Windows specific property editors             }
{*******************************************************}

unit uVCLEditors;

interface

uses
  Messages, Types, Classes, Windows, Graphics, Menus, Controls, Forms, StdCtrls,
  uDesignIntf, uDesignEditors, uDesignMenus,ActnListXE, ComCtrls;

const
  CM_FORMMODIFIED = CM_BASE + 100;

{ Property Editors }

type
{ ICustomPropertyDrawing
  Implementing this interface allows a property editor to take over the object
  inspector's drawing of the name and the value. If paFullWidthName is returned
  by IProperty.GetAttributes then only PropDrawName will be called. Default
  implementation of both these methods are provided in DefaultPropertyDrawName
  and DefaultPropertyDrawValue in this unit. }
  ICustomPropertyDrawing = interface
    ['{E1A50419-1288-4B26-9EFA-6608A35F0824}']
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

{ ICustomPropertyDrawing80
  Use this interface in addition to the above interface to control the region
  in which the custom painting occurs.  This allows only part of the value or
  the name to be custom painted.  It also allows the embedded edit control to
  to be moved in order to paint next to it. When PropDrawxxxRect is called,
  return the rectangle in which custom painting is to be done. If the rect that
  is returned is empty, then no custom painting will take place for that item.
  For PropDrawValueRect, if you do return a smaller rectangle than the one
  given, then when this item is selected, the embedded edit control will be
  placed in the remaining rect, so be sure to leave enough room so the user can
  effectively type in the edit control.  Returning the same rectangle as was
  passed in, will cause the edit control to revert to the previous behaviour of
  the edit control covering the entire value area and PropDrawValue must paint
  then entire rect.}
  ICustomPropertyDrawing80 = interface(ICustomPropertyDrawing)
    ['{73100176-DF0B-4900-AA52-4E67D7D04895}']
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

{ ICustomPropertyMessage
  Implement this interface in order for the given property to handle mouse and
  other messages (such as CMHintShow).  If the implementor handles the message,
  set Handled to true before returning to the caller.  If Handled returns true
  the default processing is *not* done. }
  ICustomPropertyMessage = interface
    ['{60E3EB3A-23DD-459C-8936-0607A27D11A8}']
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
  end;

{ ICustomPropertyDrawing
  Implementing this interface allows a property editor to take over the drawing
  of the drop down list box displayed by the property editor. This is only
  meaningful to implement if the property editor returns paValueList from
  IProperty.GetAttributes. The Value parameter is the result of
  IProperty.GetValue. The implementations ListMeasureWidth and ListMeasureHeight
  can be left blank since the var parameter is filled in to reasonable defaults
  by the object inspector. A default implementation of ListDrawValue is supplied
  in the DefaultPropertyListDrawValue procedure included in this unit }
  ICustomPropertyListDrawing = interface
    ['{BE2B8CF7-DDCA-4D4B-BE26-2396B969F8E0}']
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;


{ TFontNameProperty
  Editor for the TFont.FontName property.  Displays a drop-down list of all
  the fonts known by Windows.  The following global variable will make
  this property editor actually show examples of each of the fonts in the
  drop down list.  We would have enabled this by default but it takes
  too many cycles on slower machines or those with a lot of fonts.  Enable
  it at your own risk. ;-}
var
  FontNamePropertyDisplayFontNames: Boolean = False;

type
  TFontNameProperty = class(TStringProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;

    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

{ TFontCharsetProperty
  Editor for the TFont.Charset property.  Displays a drop-down list of the
  character-set by Windows.}

  TFontCharsetProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TImeNameProperty
  Editor for the TImeName property.  Displays a drop-down list of all
  the IME names known by Windows.}

  TImeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TColorProperty
  Property editor for the TColor type.  Displays the color as a clXXX value
  if one exists, otherwise displays the value as hex.  Also allows the
  clXXX value to be picked from a list. }

  TColorProperty = class(TIntegerProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing, ICustomPropertyDrawing80)
  protected
    function PaintColorBox(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean): TRect;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

{ TBrushStyleProperty
  Property editor for TBrush's Style.  Simply provides for custom render. }

  TBrushStyleProperty = class(TEnumProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

{ TPenStyleProperty
  Property editor for TPen's Style.  Simply provides for custom render. }

  TPenStyleProperty = class(TEnumProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    procedure GetValues(Proc: TGetStrProc); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

{ TCursorProperty
  Property editor for the TCursor type.  Displays the cursor as a clXXX value
  if one exists, otherwise displays the value as hex.  Also allows the
  clXXX value to be picked from a list. }

  TCursorProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); 
  end;

{ TFontProperty
  Property editor for the Font property.  Brings up the font dialog as well as
  allowing the properties of the object to be edited. }

  TFontProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TModalResultProperty }

  TModalResultProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TShortCutProperty
  Property editor the ShortCut property.  Allows both typing in a short
  cut value or picking a short-cut value from a list. }

  TShortCutProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TMPFilenameProperty
  Property editor for the TMediaPlayer.  Displays an File Open Dialog
  for the name of the media file.}

  TMPFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TTabOrderProperty
  Property editor for the TabOrder property.  Prevents the property from being
  displayed when more than one component is selected. }

  TTabOrderProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TCaptionProperty
  Property editor for the Caption and Text properties.  Updates the value of
  the property for each change instead on when the property is approved. }

  TCaptionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

function GetDisplayValue(const Prop: IProperty): string;
procedure DefaultPropertyDrawName(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyDrawValue(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean); overload;
procedure DefaultPropertyListDrawValue(const Value: WideString; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean); overload;


{ TBooleanProperty
  Property editor for boolean properties.  Uses a checkbox to display the state
  of boolean properties and allows easier access to toggling them. }
type
  TBooleanProperty = class(TEnumProperty,
    ICustomPropertyDrawing, ICustomPropertyDrawing80,
    ICustomPropertyMessage)
  protected
    function CBRect(const ItemRect: TRect) : TRect;
  public
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    // ICustomPropertyDrawing80
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
    // ICustomPropertyMessage
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
  end;


{ TSetProperty
  Override functionality of DesignEditors.TSetProperty to provide checkboxes for
  each set element similar to the Boolean property above }
  TSetProperty = class(uDesignEditors.TSetProperty)
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TSetElementProperty = class(uDesignEditors.TSetElementProperty,
    ICustomPropertyDrawing, ICustomPropertyDrawing80,
    ICustomPropertyMessage)
  private
    FBit : TBit;
  protected
    function CBRect(const ItemRect: TRect) : TRect;
  public
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    // ICustomPropertyDrawing80
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
    // ICustomPropertyMessage
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer);
      reintroduce;
  end;

{ TDateProperty
  Property editor for date portion of TDateTime type. }

  TDateProperty = class(uDesignEditors.TDateProperty, IProperty80)
  private
    FControl : TMonthCalendar;
    FHost: IPropertyHost;
    FIgnoreDblClick: Boolean;
  public
    destructor Destroy; override;

    function GetAttributes: TPropertyAttributes; override;

    procedure Edit(const Host: IPropertyHost; DblClick: Boolean); reintroduce; overload;

    procedure CalendarDblClick(Sender: TObject);
    procedure CalendarKeyPress(Sender: TObject; var Key: Char);
  end;

{ TActionListView }

  TNewActionEvent = procedure(Sender: TObject; const Category: string;
    ActionClass: TContainedActionClass; ActionList: TCustomActionList) of object;
  TSelectActionEvent = procedure(Sender: TObject; Action: TContainedAction) of object;

  TActionListView = class(TCustomListView)
  private const
    FDefItemHeight = 17;
  private
    FActionList: TCustomActionList;
    FDesigner: IDesigner;
    FImageList: TImageList;
    FNewActnPopupMenu: TPopupMenu;
    FNewStdActnPopupMenu: TPopupMenu;
    FStdActionList: TStrings;
    FTempStringList: TStrings;
    FOnNewAction: TNewActionEvent;
    FOnSelectAction: TSelectActionEvent;
    procedure AddStdAction(const Category: string;
      ActionClass: TBasicActionClass; Info: Pointer);
    procedure AddTempString(const S: string);
    function CreateMenuItem(const Caption: string;
      Event: TNotifyEvent; CustomData: Pointer): TMenuItem;
    procedure DoNewActionClick(Sender: TObject);
    procedure DoNewStdActionClick(Sender: TObject);
    procedure RebuildListView;
    procedure RebuildPopupMenus;
    procedure SetDesigner(const Value: IDesigner);
    procedure ShowPopupMenu(Item: TListItem; PopupMenu: TPopupMenu);
  protected
    procedure CreateWnd; override;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Designer: IDesigner read FDesigner write SetDesigner;
    property OnNewAction: TNewActionEvent read FOnNewAction write FOnNewAction;
    property OnSelectAction: TSelectActionEvent read FOnSelectAction write FOnSelectAction;
  end;

{ TActionProperty }

  TActionProperty = class(TComponentProperty, IProperty80)
  private
    FActionListView: TActionListView;
    FHost: IPropertyHost;
    procedure CreateNewAction(Sender: TObject; const Category: string;
      ActionClass: TContainedActionClass; ActionList: TCustomActionList);
    procedure SelectAction(Sender: TObject; Action: TContainedAction);
  public
    destructor Destroy; override;
    // IProperty80
    procedure Edit(const Host: IPropertyHost; DblClick: Boolean); reintroduce; overload;
    function GetAttributes: TPropertyAttributes; override;
  end;


type
{ ISelectionMessage }

{ If a selection editor implements this interface the form designer will ensure
  all windows message are first sent through this interface before handling
  them when the selection editor for the corresponding class is selected.

  IsSelectionMessage - Filter for all messages processed by the designer when
    this the implementing selection editor is active. Return True if the message
    is handled by the selection editor which causes the designer to ignore
    the message (as well as preventing the control from seeing the message)
    or False, allowing the designer to process the message normally.
      Sender   the control that received the original message.
      Message  the message sent by windows to the control. }
  ISelectionMessage = interface
    ['{58274878-BB87-406A-9220-904105C9E112}']
    function IsSelectionMessage(Sender: TControl;
      var Message: TMessage): Boolean;
  end;

  ISelectionMessageList = interface
    ['{C1360368-0099-4A7C-A4A8-7650503BA0C6}']
    function Get(Index: Integer): ISelectionMessage;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ISelectionMessage read Get; default;
  end;

function SelectionMessageListOf(const SelectionEditorList: ISelectionEditorList): ISelectionMessageList;

{ Custom Module Types }

type

{ ICustomDesignForm
  Allows a custom module to create a different form for use by the designer
  as the base form.

    CreateDesignForm - Create a descendent of TCustomForm for use by the
      designer as the instance to design }
  ICustomDesignForm = interface
    ['{787195AF-C234-49DC-881B-221B69C0137A}']
    procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
      out DesignForm: TCustomForm; out ComponentContainer: TWinControl); deprecated;
  end;

  ICustomDesignForm80 = interface
    ['{525A1DF8-5EF3-4B98-8EE3-4567910A7EA1}']
    procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
      out DesignForm: IHostForm; out ComponentContainer: TWinControl);
  end;

  TControlGuidelines = class(TComponentGuidelines)
  private
    FClientDelta: TRect;
    FBoundsRect: TRect; 
  protected
    procedure CalcClientDelta(Force: Boolean = True);
    function CalcHorzPos(APos: Integer): Integer; override;
    function CalcVertPos(APos: Integer): Integer; override;
    function GetCount: Integer; override;
    function GetDesignerGuideType(Index: Integer): TDesignerGuideType; override;
    function GetDesignerGuideOffset(Index: Integer): Integer; override;
    function GetCtl3D: Boolean; virtual;
  public
    procedure Initialize(AComponent: TComponent; AContainer: TComponent); override;
    class function GetTextBaseline(AControl: TControl; Align: TTextLayout): Integer; static;
  end;

  TWinControlGuidelines = class(TControlGuidelines)
  private
    FCountDelta: Integer;
  protected
    function GetControlPadding: TPadding; virtual;
    function GetCount: Integer; override;
    function GetDesignerGuideType(Index: Integer): TDesignerGuideType; override;
    function GetDesignerGuideOffset(Index: Integer): Integer; override;
  public
    procedure Initialize(AComponent: TComponent; AContainer: TComponent); override;
  end;

  TCustomFormGuidelines = class(TWinControlGuidelines)
  private
    FCustomPadding: TPadding;
  protected
    function GetControlPadding: TPadding; override;
  public
    destructor Destroy; override;
  end;

{ Clipboard utility functions }

const
  cfDelphiComponents = 'Delphi Components';
  cfDelphiComponent = 'Delphi Component';

var
  CF_COMPONENTS: Word;
  CF_COMPONENT: Word;

procedure CopyStreamToClipboard(S: TMemoryStream);
function GetClipboardStream: TMemoryStream;

{ EditAction utility functions }

function EditActionFor(AEditControl: TCustomEdit; Action: TEditAction): Boolean;
function GetEditStateFor(AEditControl: TCustomEdit): TEditState;

{ Registry Information }

var
  BaseRegistryKey: string = '';

{ Action Registration }

type

  TNotifyActionListChange = procedure;

var
  NotifyActionListChange: TNotifyActionListChange = nil;

procedure RegActions(const ACategory: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
procedure UnRegActions(const Classes: array of TBasicActionClass);
procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction;

implementation

uses Consts, RTLConsts, SysUtils, Math, Dialogs, Registry, TypInfo,
     Clipbrd, ImgList, CommCtrl, Themes, Generics.Collections, uDsnConst,
     UxTheme, GraphUtil, uDesignConst, uComponentDesigner;

{ Registry Information }

type

  TBasicActionRecord = record
    ActionClass: TBasicActionClass;
    GroupId: Integer;
  end;

  TActionClassArray = array of TBasicActionRecord;

  TActionClassesEntry = record
    Category: string;
    Actions: TActionClassArray;
    Resource: TComponentClass;
  end;

  TActionClassesArray = array of TActionClassesEntry;

  TActionResourceCache = class
  private
    type
      TResourceCache = TDictionary<TComponentClass, TComponent>;
    class var FCache: TResourceCache;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Add(ComponentClass: TComponentClass; Instance: TComponent);
    class procedure Clear;
    class function GetInstance(ComponentClass: TComponentClass): TComponent;
    class procedure Remove(ComponentClass: TComponentClass);
  end;

var
  DesignersList: TList = nil;
  ActionClasses: TActionClassesArray = nil;

{ Action Registration }

type
  THackAction = class(TCustomAction);

procedure RegActions(const ACategory: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
var
  CategoryIndex, Len, I, J, NewClassCount: Integer;
  NewClasses: array of TBasicActionClass;
  Skip: Boolean;
  S: string;
begin
  // Remove resource from cache if it's there
  if TActionResourceCache.GetInstance(AResource) <> nil then
    TActionResourceCache.Remove(AResource);

  { Determine whether we're adding a new category, or adding to an existing one }
  CategoryIndex := -1;
  for I := Low(ActionClasses) to High(ActionClasses) do
    if CompareText(ActionClasses[I].Category, ACategory) = 0 then
    begin
      CategoryIndex := I;
      Break;
    end;

  { Adding a new category }
  if CategoryIndex = -1 then
  begin
    CategoryIndex := Length(ActionClasses);
    SetLength(ActionClasses, CategoryIndex + 1);
  end;

  with ActionClasses[CategoryIndex] do
  begin
    SetLength(NewClasses, Length(AClasses));
    { Remove duplicate classes }
    NewClassCount := 0;
    for I := Low(AClasses) to High(AClasses) do
    begin
      Skip := False;
      for J := Low(Actions) to High(Actions) do
        if AClasses[I] = Actions[J].ActionClass then
        begin
          Skip := True;
          Break;
        end;
      if not Skip then
      begin
        NewClasses[Low(NewClasses) + NewClassCount] := AClasses[I];
        Inc(NewClassCount);
      end;
    end;

    { Pack NewClasses }
    SetLength(NewClasses, NewClassCount);

    SetString(S, PChar(ACategory), Length(ACategory));
    Category := S;
    Resource := AResource;
    Len := Length(Actions);
    SetLength(Actions, Len + Length(NewClasses));
    for I := Low(NewClasses) to High(NewClasses) do
    begin
      RegisterNoIcon([NewClasses[I]]);
      Classes.RegisterClass(NewClasses[I]);
      with Actions[Len + I] do
      begin
        ActionClass := NewClasses[I];
        GroupId := CurrentGroup;
      end;
    end;
  end;
  { Notify all available designers of new TAction class }
  if (DesignersList <> nil) and Assigned(NotifyActionListChange) then
    NotifyActionListChange;
end;

procedure UnRegActions(const Classes: array of TBasicActionClass);//! far;
var
  I, J, K: Integer;
  LActionClass: TBasicActionClass;
begin
  // Clear the resource cache
  TActionResourceCache.Clear;

  for I := Low(Classes) to High(Classes) do
  begin
    LActionClass := Classes[I];
    for J := Low(ActionClasses) to High(ActionClasses) do
      for K := Low(ActionClasses[J].Actions) to High(ActionClasses[J].Actions) do
        with ActionClasses[J].Actions[K] do
          if LActionClass = ActionClass then
          begin
            ActionClass := nil;
            GroupId := -1;
          end;
  end;
  if Assigned(NotifyActionListChange) then
    NotifyActionListChange;
end;

procedure UnregisterActionGroup(AGroupId: Integer);
var
  I, J: Integer;
begin
  for I := Low(ActionClasses) to High(ActionClasses) do
    for J := Low(ActionClasses[I].Actions) to High(ActionClasses[I].Actions) do
      with ActionClasses[I].Actions[J] do
        if GroupId = AGroupId then
        begin
          ActionClass := nil;
          GroupId := -1;
        end;
  if Assigned(NotifyActionListChange) then
    NotifyActionListChange;
end;

procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
var
  I, J, Count: Integer;
  ActionClass: TBasicActionClass;
begin
  if ActionClasses <> nil then
    for I := Low(ActionClasses) to High(ActionClasses) do
    begin
      Count := 0;
      for J := Low(ActionClasses[I].Actions) to High(ActionClasses[I].Actions) do
      begin
        ActionClass := ActionClasses[I].Actions[J].ActionClass;
        if ActionClass = nil then
          Continue;
        Proc(ActionClasses[I].Category, ActionClass, Info);
        Inc(Count);
      end;
      if Count = 0 then
        SetLength(ActionClasses[I].Actions, 0);
    end;
end;

function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction;
var
  I, J: Integer;
  Res: TComponentClass;
  Instance: TComponent;
  Action: TBasicAction;

  function FindComponentByClass(AOwner: TComponent; const AClassName: string): TComponent;
  var
    I: Integer;
  begin
    if (AClassName <> '') and (AOwner.ComponentCount > 0) then
      for I := 0 to AOwner.ComponentCount - 1 do
      begin
        Result := AOwner.Components[I];
        if CompareText(Result.ClassName, AClassName) = 0 then Exit;
      end;
    Result := nil;
  end;

  procedure CreateMaskedBmp(ImageList: TCustomImageList; ImageIndex: Integer;
    var Image, Mask: Graphics.TBitmap);
  begin
    Image := Graphics.TBitmap.Create;
    Mask := Graphics.TBitmap.Create;
    try
      with Image do
      begin
        Height := ImageList.Height;
        Width := ImageList.Width;
      end;
      with Mask do
      begin
        Monochrome := True;
        Height := ImageList.Height;
        Width := ImageList.Width;
      end;
      ImageList_Draw(ImageList.Handle, ImageIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
      ImageList_Draw(ImageList.Handle, ImageIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
//!      Result.MaskHandle := Mask.ReleaseHandle;
    except
      Image.Free;
      Mask.Free;
      Image := nil;
      Mask := nil;
      raise;
    end;
  end;

begin
  Result := ActionClass.Create(AOwner);
  { Attempt to find the first action with the same class Type as ActionClass in
    the Resource component's resource stream, and use its property values as
    our defaults. }
  Res := nil;
  for I := Low(ActionClasses) to High(ActionClasses) do
    with ActionClasses[I] do
      for J := Low(Actions) to High(Actions) do
        if Actions[J].ActionClass = ActionClass then
        begin
          Res := Resource;
          Break;
        end;
  if Res <> nil then
  begin
    // Look for this resource in the cache
    Instance := TActionResourceCache.GetInstance(Res);
    if Instance = nil then
    begin
      // Not found, create it and add it
      Instance := Res.Create(nil);
      TActionResourceCache.Add(Res, Instance);
    end;

    Action := FindComponentByClass(Instance, ActionClass.ClassName) as TBasicAction;
    if Action <> nil then
    begin
      with Action as TCustomAction do
      begin
        TCustomAction(Result).Caption := Caption;
        TCustomAction(Result).Checked := Checked;
        TCustomAction(Result).Enabled := Enabled;
        TCustomAction(Result).HelpContext := HelpContext;
        TCustomAction(Result).Hint := Hint;
        TCustomAction(Result).ImageIndex := ImageIndex;
        TCustomAction(Result).ShortCut := ShortCut;
        TCustomAction(Result).Visible := Visible;
        if (ImageIndex > -1) and (ActionList <> nil) and
          (ActionList.Images <> nil) then
        begin
          THackAction(Result).FImage.Free;
          THackAction(Result).FMask.Free;
          CreateMaskedBmp(ActionList.Images, ImageIndex,
            Graphics.TBitmap(THackAction(Result).FImage),
            Graphics.TBitmap(THackAction(Result).FMask));
        end;
      end;
    end;
  end;
end;

const
  { context ids for the Font editor and the Color Editor, etc. }
  hcDFontEditor       = 25000;
  hcDColorEditor      = 25010;
  hcDMediaPlayerOpen  = 25020;

function GetDisplayValue(const Prop: IProperty): string;
begin
  Result := '';
  if Assigned(Prop) and Prop.AllEqual then
    Result := Prop.GetValue;
end;

procedure DefaultPropertyDrawName(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
begin
  Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 1, Prop.GetName);
end;

procedure DefaultPropertyDrawValue(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Prop.GetVisualValue);
end;

procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean);
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Value);
end;

// Copy from IDEWideGraphics
type
  TCanvasClass = Class(TCanvas);

function TextExtentW(Canvas: TCanvas; const Text: Widestring): TSize;
begin
  with TCanvasClass(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid]);
    Result.cX := 0;
    Result.cY := 0;
    Windows.GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result);
    Changed;
  end;
end;

function TextWidthW(Canvas: TCanvas; const Text: Widestring): Integer; inline;
begin
  Result := TextExtentW(Canvas, Text).cX;
end;

procedure TextRectW(Canvas: TCanvas; Rect: TRect; X, Y: Integer; 
  const Text: WideString);
var
  Options: Longint;
begin
  with TCanvasClass(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and
       (CanvasOrientation = coRightToLeft) then Inc(X, TextWidthW(Canvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
      Length(Text), nil);
    Changed;
  end;
end;

procedure DefaultPropertyListDrawValue(const Value: WideString; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean);
begin
  TextRectW(Canvas, Rect, Rect.Left + 1, Rect.Top + 1, Value);
end;

{ TFontNameProperty }
{ Owner draw code has been commented out, see the interface section's for info. }

function TFontNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TFontNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count - 1 do Proc(Screen.Fonts[I]);
end;

procedure TFontNameProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  OldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      OldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      TextRect(ARect, ARect.Left + 2, ARect.Top + 1, Value);

      // restore things
      Font.Name := OldFontName;
    end
  else
    DefaultPropertyListDrawValue(Value, ACanvas, ARect, ASelected);
end;

procedure TFontNameProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  OldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      OldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      AHeight := TextHeight(Value) + 2;

      // restore things
      Font.Name := OldFontName;
    end;
end;

procedure TFontNameProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  OldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      OldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      AWidth := TextWidth(Value) + 4;

      // restore things
      Font.Name := OldFontName;
    end;
end;

{ TFontCharsetProperty }

function TFontCharsetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList];
end;

function TFontCharsetProperty.GetValue: string;
begin
  if not CharsetToIdent(TFontCharset(GetOrdValue), Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TFontCharsetProperty.GetValues(Proc: TGetStrProc);
begin
  GetCharsetValues(Proc);
end;

procedure TFontCharsetProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCharset(Value, NewValue) then
    SetOrdValue(NewValue)
  else inherited SetValue(Value);
end;

{ TImeNameProperty }

function TImeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TImeNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Imes.Count - 1 do Proc(Screen.Imes[I]);
end;

{ TMPFilenameProperty }

procedure TMPFilenameProperty.Edit;
var
  MPFileOpen: TOpenDialog;
begin
  MPFileOpen := TOpenDialog.Create(Application);
  MPFileOpen.Filename := GetValue;
  MPFileOpen.Filter := SMPOpenFilter;
  MPFileOpen.HelpContext := hcDMediaPlayerOpen;
  MPFileOpen.Options := MPFileOpen.Options + [ofShowHelp, ofPathMustExist,
    ofFileMustExist];
  try
    if MPFileOpen.Execute then SetValue(string(MPFileOpen.Filename));
  finally
    MPFileOpen.Free;
  end;
end;

function TMPFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{ TColorProperty }

procedure TColorProperty.Edit;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S,
              CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.HelpContext := hcDColorEditor;
    ColorDialog.Options := [cdShowHelp];
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TColorProperty.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TColorProperty.GetValues(Proc: TGetStrProc);
begin
  GetColorValues(Proc);
end;

procedure TColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    PaintColorBox(GetVisualValue, ACanvas, ARect, ASelected)
//    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  ValueRect: TRect;
begin
  ValueRect := PaintColorBox(Value, ACanvas, ARect, ASelected);
  DefaultPropertyListDrawValue(Value, ACanvas, ValueRect, ASelected);
end;

procedure TColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TColorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implemenation necessary
end;

procedure TColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TColorProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TColorProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

function TColorProperty.PaintColorBox(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean): TRect;

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up and do the work
    Brush.Color := StringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    Result := Rect(Right, ARect.Top, ARect.Right, ARect.Bottom);
{    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top, ARect.Right,
      ARect.Bottom), ASelected);}
  end;
end;

{ TBrushStyleProperty }

procedure TBrushStyleProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TBrushStyleProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldBrushStyle := Brush.Style;

    // frame things
    Pen.Color := Brush.Color;
    Brush.Color := clWindow;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up
    Pen.Color := clWindowText;
    Brush.Style := TBrushStyle(GetEnumValue(GetPropInfo^.PropType^, Value));

    // bsClear hack
    if Brush.Style = bsClear then
    begin
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := clWindowText;

    // ok on with the show
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TBrushStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('A') {* 2};
end;

procedure TBrushStyleProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implementation necessary
end;

procedure TBrushStyleProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{ TPenStyleProperty }

procedure TPenStyleProperty.GetValues(Proc: TGetStrProc);
var
  LStyle: TPenStyle;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  for LStyle := Low(TPenStyle) to High(TPenStyle) do
    if (LStyle <> psUserStyle) and (LStyle <> psAlternate) then
      Proc(GetEnumName(EnumType, Integer(LStyle)));
end;

procedure TPenStyleProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TPenStyleProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right, Top: Integer;
  OldPenColor, OldBrushColor: TColor;
  OldPenStyle: TPenStyle;
begin
  Right := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
  Top := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldPenStyle := Pen.Style;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // white out the background
    Pen.Color := clWindowText;
    Brush.Color := clWindow;
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // set thing up and do work
    Pen.Color := clWindowText;
    Pen.Style := TPenStyle(GetEnumValue(GetPropInfo^.PropType^, Value));
    MoveTo(ARect.Left + 1, Top);
    LineTo(Right - 1, Top);
    MoveTo(ARect.Left + 1, Top + 1);
    LineTo(Right - 1, Top + 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Style := OldPenStyle;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TPenStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('X') * 2;
end;

procedure TPenStyleProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implementation necessary
end;

procedure TPenStyleProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{ TCursorProperty }

function TCursorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TCursorProperty.GetValue: string;
begin
  Result := CursorToString(TCursor(GetOrdValue));
end;

procedure TCursorProperty.GetValues(Proc: TGetStrProc);
begin
  GetCursorValues(Proc);
end;

procedure TCursorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCursor(Value, NewValue) then
    SetOrdValue(NewValue)
  else inherited SetValue(Value);
end;

procedure TCursorProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  CursorIndex: Integer;
  CursorHandle: THandle;
begin                                            
  Right := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;
  with ACanvas do
  begin
    if not IdentToCursor(Value, CursorIndex) then
      CursorIndex := StrToInt(Value);
    ACanvas.FillRect(ARect);
    CursorHandle := Screen.Cursors[CursorIndex];
    if CursorHandle <> 0 then
      DrawIconEx(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, CursorHandle,
        0, 0, 0, 0, DI_NORMAL or DI_DEFAULTSIZE);
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TCursorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

procedure TCursorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight('Wg'), GetSystemMetrics(SM_CYCURSOR) + 4);
end;

{ TFontProperty }

procedure TFontProperty.Edit;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist];
    if FontDialog.Execute then SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

function TFontProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TModalResultProperty }

const

  mrNone     = 0;
  mrClose    = 11;
  ModalResults: array[mrNone..mrClose] of string = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll',
    'mrClose');

function TModalResultProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TModalResultProperty.GetValue: string;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do Proc(ModalResults[I]);
end;

procedure TModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
  begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(ModalResults) to High(ModalResults) do
    if CompareText(ModalResults[I], Value) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

{ TShortCutProperty }

const
  ShortCuts: array[0..108] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);

function TShortCutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TShortCutProperty.GetValue: string;
var
  CurValue: TShortCut;
begin
  CurValue := GetOrdValue;
  if CurValue = scNone then
    Result := srNone else
    Result := ShortCutToText(CurValue);
end;

procedure TShortCutProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(srNone);
  for I := 1 to High(ShortCuts) do Proc(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutProperty.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, srNone) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(NewValue);
end;

{ TTabOrderProperty }

function TTabOrderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

{ TCaptionProperty }

function TCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paAutoUpdate, paRevertable];
end;

{ TBooleanProperty }

procedure DrawCheckbox(ACanvas: TCanvas; ARect : TRect;
  ASelected, AEnabled, AAllEqual, AValue: Boolean);
const
  ThemeStyles : array[TCheckBoxState] of array[Boolean] of TThemedButton =
  ((
    tbCheckBoxUncheckedDisabled,
    tbCheckBoxUnCheckedNormal
  ),(
    tbCheckBoxCheckedDisabled,
    tbCheckBoxCheckedNormal
  ),(
    tbCheckBoxMixedDisabled,
    tbCheckBoxMixedNormal
  ));

  UnThemedStyles : array[TCheckBoxState] of array[Boolean] of Cardinal =
  ((
    DFCS_BUTTONCHECK or DFCS_INACTIVE,
    DFCS_BUTTONCHECK
  ),(
    DFCS_CHECKED or DFCS_INACTIVE,
    DFCS_CHECKED
  ),(
    DFCS_BUTTON3STATE or DFCS_INACTIVE,
    DFCS_BUTTON3STATE
  ));
var State : TCheckBoxState;
begin
  if AAllEqual = false then
    State := cbGrayed
  else if AValue then
    State := cbChecked
  else
    State := cbUnchecked;

  if ThemeServices.ThemesEnabled then
    ThemeServices.DrawElement(ACanvas.Handle,
      ThemeServices.GetElementDetails(ThemeStyles[State][AEnabled]), ARect)
  else
    DrawFrameControl(ACanvas.Handle, ARect,
      DFC_BUTTON, UnThemedStyles[State][AEnabled]);
end;

function TBooleanProperty.CBRect(const ItemRect: TRect): TRect;
begin
  Result := Rect(ItemRect.Right + 2, ItemRect.Top,
    itemrect.Right + Itemrect.Bottom - ItemRect.Top + 2, ItemRect.Bottom);
end;

procedure TBooleanProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  uVCLEditors.DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TBooleanProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

procedure TBooleanProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DrawCheckbox(ACanvas, ARect, ASelected, not (paReadOnly in GetAttributes),
    AllEqual, Boolean(GetOrdValue()));
end;

function TBooleanProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

procedure TBooleanProperty.HintShow(var HintInfo: THintInfo;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseMove(Shift: TShiftState; X, Y: Integer;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
  if paReadOnly in GetAttributes then Exit;
  if PtInRect(CBRect(ItemRect), Point(x,y)) then
  begin
    SetOrdValue(1-GetOrdValue());
    Handled := True;
  end;
end;

{ TSetProperty }

procedure TSetProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  E: IProperty;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType^)^ do
    for I := MinValue to MaxValue do
    begin
      { Fix addref problems by referencing it here }
      E := TSetElementProperty.Create(Self, I);
      Proc(E);
      E := nil;
    end;
end;

function SetPropMapper(Obj: TPersistent; PropInfo: PPropInfo): TPropertyEditorClass;
begin
  Result := nil;
  if PropInfo.PropType^.Kind = tkSet then
    Result := TSetProperty;
end;

{ TSetElementProperty }

function TSetElementProperty.CBRect(const ItemRect: TRect): TRect;
begin
  Result := Rect(ItemRect.Right + 2, ItemRect.Top,
    itemrect.Right + Itemrect.Bottom - ItemRect.Top + 2, ItemRect.Bottom);
end;

constructor TSetElementProperty.Create(Parent: TPropertyEditor; AElement: Integer);
var
  MinValue: integer;
begin
  inherited;
  MinValue := GetTypeData(GetTypeData(GetPropType).CompType^).MinValue;
  FBit := AElement - MinValue;
end;

procedure TSetElementProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  uVCLEditors.DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TSetElementProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

procedure TSetElementProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  DrawCheckbox(ACanvas, ARect, ASelected, not (paReadOnly in GetAttributes),
    AllEqual, FBit in S);
end;

function TSetElementProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

procedure TSetElementProperty.HintShow(var HintInfo: THintInfo;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TSetElementProperty.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TSetElementProperty.MouseMove(Shift: TShiftState; X, Y: Integer;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TSetElementProperty.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
var
  S: TIntegerSet;
begin
  Handled := False;
  if paReadOnly in GetAttributes then Exit;
  if PtInRect(CBRect(ItemRect), Point(x,y)) then
  begin
    Integer(S) := GetOrdValue;
    if FBit in S then
      Exclude(S, FBit)
    else
      Include(S, FBit);
    SetOrdValue(Integer(S));
    Handled := True;
  end;
end;

{ TDateProperty }

destructor TDateProperty.Destroy;
begin
  if FControl <> nil then
    FControl.Free;
  inherited;
end;

function TDateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited + [paCustomDropDown];
end;

procedure TDateProperty.Edit(const Host: IPropertyHost; DblClick: Boolean);
var
  R : TRect;
  Val : Single;
begin
  if FControl = nil then
    FControl := TMonthCalendar.Create(nil);
  Fcontrol.OnKeyPress := CalendarKeyPress;
  FControl.OnDblClick := CalendarDblClick;
  fcontrol.Align := alNone;
  FControl.Visible := False;
  FControl.ParentWindow := GetDesktopWindow;
  MonthCal_GetMinReqRect(fcontrol.Handle, R);
  fcontrol.ParentWindow := 0;
  FControl.Visible := True;
  FControl.Width := R.Right;
  FControl.Height := R.Bottom;
  Val := GetFloatValue;
  if Val = 0 then
    FControl.Date := Now
  else
    FControl.Date := Val;
  Fcontrol.Update;
  FHost := Host;
  Host.DropDownControl(FControl);
end;

procedure TDateProperty.CalendarDblClick(Sender: TObject);
begin
  if not FIgnoreDblClick then
    SetFloatValue(TMonthCalendar(Sender).Date);
end;

procedure TDateProperty.CalendarKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    #27: FHost.CloseDropDown;
    #13: SetFloatValue(FControl.Date);
  end;
end;

{ Clipboard routines }

procedure CopyStreamToClipboard(S: TMemoryStream);
var
  T: TMemoryStream;
  I: TValueType;
  V: Integer;

  procedure CopyToClipboard(Format: Word; S: TMemoryStream);
  var
    Handle: THandle;
    Mem: Pointer;
  begin
    Handle := GlobalAlloc(GMEM_MOVEABLE, S.Size);
    Mem := GlobalLock(Handle);
    Move(S.Memory^, Mem^, S.Size);
    GlobalUnlock(Handle);
    Clipboard.SetAsHandle(Format, Handle);
  end;

begin
  Clipboard.Open;
  try
    CopyToClipboard(CF_COMPONENTS, S);
    S.Position := 0;
    T := TMemoryStream.Create;
    try
      repeat
        S.Read(I, SizeOf(I));
        S.Seek(-SizeOf(I), 1);
        if I = vaNull then Break;
        ObjectBinaryToText(S, T);
      until False;
      V := 0;
      T.Write(V, 1);
      CopyToClipboard(CF_TEXT, T);
    finally
      T.Free;
    end;
  finally
    Clipboard.Close;
  end;
end;

function GetClipboardStream: TMemoryStream;
var
  S, T: TMemoryStream;
  Handle: THandle;
  Mem: Pointer;
  Format: Word;
  V: TValueType;

  function AnotherObject(S: TStream): Boolean;
  var
    Buffer: array[0..255] of AnsiChar;
    Position: Integer;
  begin
    Position := S.Position;
    Buffer[S.Read(Buffer, SizeOf(Buffer))-1] := #0;
    S.Position := Position;
    Result := PossibleStream(string(Buffer));
  end;

begin
  Result := TMemoryStream.Create;
  try
    if Clipboard.HasFormat(CF_COMPONENTS) then
      Format := CF_COMPONENTS else
      Format := CF_TEXT;
    Clipboard.Open;
    try
      Handle := Clipboard.GetAsHandle(Format);
      Mem := GlobalLock(Handle);
      try
        Result.Write(Mem^, GlobalSize(Handle));
      finally
        GlobalUnlock(Handle);
      end;
    finally
      Clipboard.Close;
    end;
    Result.Position := 0;
    if Format = CF_TEXT then
    begin
      S := TMemoryStream.Create;
      try
        while AnotherObject(Result) do ObjectTextToBinary(Result, S);
        V := vaNull;
        S.Write(V, SizeOf(V));
        T := Result;
        Result := nil;
        T.Free;
      except
        S.Free;
        raise;
      end;
      Result := S;
      Result.Position := 0;
    end;
  except
    Result.Free;
    raise;
  end;
end;

type
  TSelectionMessageList = class(TInterfacedObject, ISelectionMessageList)
  private
    FList: IInterfaceList;
  protected
    procedure Add(AEditor: ISelectionMessage);
  public
    constructor Create;
    function Get(Index: Integer): ISelectionMessage;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ISelectionMessage read Get; default;
  end;

{ TSelectionMessageList }

procedure TSelectionMessageList.Add(AEditor: ISelectionMessage);
begin
  FList.Add(AEditor);
end;

constructor TSelectionMessageList.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
end;

function TSelectionMessageList.Get(Index: Integer): ISelectionMessage;
begin
  Result := FList[Index] as ISelectionMessage;
end;

function TSelectionMessageList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function SelectionMessageListOf(const SelectionEditorList: ISelectionEditorList): ISelectionMessageList;
var
  SelectionMessage: ISelectionMessage;
  I: Integer;
  R: TSelectionMessageList;
begin
  R := TSelectionMessageList.Create;
  for I := 0 to SelectionEditorList.Count - 1 do
    if Supports(SelectionEditorList[I], ISelectionMessage, SelectionMessage) then
      R.Add(SelectionMessage);
  Result := R;
end;

{ EditAction utility functions }

function EditActionFor(AEditControl: TCustomEdit; Action: TEditAction): Boolean;
begin
  Result := True;
  case Action of
    eaUndo:      AEditControl.Undo;
    eaCut:       AEditControl.CutToClipboard;
    eaCopy:      AEditControl.CopyToClipboard;
    eaDelete:    AEditControl.ClearSelection;
    eaPaste:     AEditControl.PasteFromClipboard;
    eaSelectAll: AEditControl.SelectAll;
  else
    Result := False;
  end;
end;

function GetEditStateFor(AEditControl: TCustomEdit): TEditState;
begin
  Result := [];
  if AEditControl.CanUndo then
    Include(Result, esCanUndo);
  if AEditControl.SelLength > 0 then
  begin
    Include(Result, esCanCut);
    Include(Result, esCanCopy);
    Include(Result, esCanDelete);
  end;
  if Clipboard.HasFormat(CF_TEXT) then
    Include(Result, esCanPaste);
  if AEditControl.SelLength < Length(AEditControl.Text) then
    Include(Result, esCanSelectAll);
end;

{ TControlGuidelines }

function TControlGuidelines.CalcVertPos(APos: Integer): Integer;
var
  LParent: TWinControl;
  LPoint: TPoint;
begin
  if Component <> Container then
  begin
    LParent := TControl(Component).Parent;
    LPoint := Point(0, TControl(Component).Top);
    LPoint := LParent.ClientToScreen(LPoint);
    LPoint := TControl(Container).ScreenToClient(LPoint);
    Result := LPoint.Y + APos;
  end
  else
    Result := TControl(Component).Top + APos;
end;

procedure TControlGuidelines.CalcClientDelta(Force: Boolean = True);
var
  Control: TControl;
  ClientRect, BoundsRect: TRect;
  Origin: TPoint;
begin
  if Component = Container then
  begin
    Control := TControl(Component);
    BoundsRect := Control.BoundsRect;
    if Force or not EqualRect(BoundsRect, FBoundsRect) then
    begin
      FBoundsRect := BoundsRect;
      ClientRect := Control.ClientRect;
      Origin := Control.ClientOrigin;
      OffsetRect(ClientRect, Origin.X, Origin.Y);
      if Control.Parent <> nil then
      begin
        BoundsRect.TopLeft := Control.Parent.ClientToScreen(BoundsRect.TopLeft);
        BoundsRect.BottomRight := Control.Parent.ClientToScreen(BoundsRect.BottomRight);
      end;
      FClientDelta.Top := Control.Top + (ClientRect.Top - BoundsRect.Top);
      FClientDelta.Left := Control.Left + (ClientRect.Left - BoundsRect.Left);
      FClientDelta.BottomRight := FClientDelta.TopLeft;
    end;
  end;
end;

function TControlGuidelines.CalcHorzPos(APos: Integer): Integer;
var
  LParent: TWinControl;
  LPoint: TPoint;
begin
  if Component <> Container then
  begin
    LParent := TControl(Component).Parent;
    LPoint := Point(TControl(Component).Left, 0);
    LPoint := LParent.ClientToScreen(LPoint);
    LPoint := TControl(Container).ScreenToClient(LPoint);
    Result := LPoint.X + APos;
  end
  else
    Result := TControl(Component).Left + APos;
end;

function TControlGuidelines.GetCount: Integer;
begin
  Result := 8;
end;

function TControlGuidelines.GetCtl3D: Boolean;
begin
  if IsPublishedProp(Component, 'Ctl3D') then  // do not localize
    Result := GetOrdProp(Component, 'Ctl3D') <> 0 // do not localize
  else
    Result := True;
end;

function TControlGuidelines.GetDesignerGuideOffset(Index: Integer): Integer;
var
  Control: TControl;
begin
  Control := TControl(Component);
  CalcClientDelta(False);
  case Index of
    0: Result := -FClientDelta.Left;
    1: Result := -FClientDelta.Top;
    2: Result := Control.Width - 1 - FClientDelta.Right;
    3: Result := Control.Height - 1 - FClientDelta.Bottom;
    4: Result := -Control.Margins.Left - FClientDelta.Left;
    5: Result := -Control.Margins.Top - FClientDelta.Top;
    6: Result := (Control.Width - FClientDelta.Right) + Control.Margins.Right;
    7: Result := (Control.Height - FClientDelta.Bottom) + Control.Margins.Bottom;
  else
    Result := 0;
    TList.Error(SListIndexError, Index);
  end;
end;

function TControlGuidelines.GetDesignerGuideType(Index: Integer): TDesignerGuideType;
const
  DesignTypes: array[0..7] of TDesignerGuideType =
    (gtAlignLeft, gtAlignTop, gtAlignRight, gtAlignBottom, gtMarginLeft, gtMarginTop, gtMarginRight, gtMarginBottom);
begin
  if (Index >= 0) and (Index < GetCount) then
    Result := DesignTypes[Index]
  else
  begin
    Result := gtAlignLeft;
    TList.Error(SListIndexError, Index);
  end;
end;

type
  TOpenControl = class(TControl);

class function TControlGuidelines.GetTextBaseline(AControl: TControl; Align: TTextLayout): Integer;
var
  Canvas: TControlCanvas;
  tm: TTextMetric;
  ClientRect: TRect;
  Ascent, Height: Integer;
begin
  Canvas := TControlCanvas.Create;
  try
    ClientRect := AControl.ClientRect;
    Canvas.Control := AControl;
    Canvas.Font := TOpenControl(AControl).Font;
    GetTextMetrics(Canvas.Handle, tm);
    Ascent := tm.tmAscent + 1;
    Height := tm.tmHeight;
    case Align of
      tlTop: Result := ClientRect.Top + Ascent;
      tlCenter: Result := (ClientRect.Top + (ClientRect.Bottom - Height) div 2) + Ascent;
      tlBottom: Result := (ClientRect.Bottom - Height) + Ascent;
    else
      Result := 0;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TControlGuidelines.Initialize(AComponent, AContainer: TComponent);
begin
  inherited Initialize(AComponent, AContainer);
  CalcClientDelta;
end;

{ TWinControlGuidelines }

function TWinControlGuidelines.GetControlPadding: TPadding;
begin
  Result := TWinControl(Component).Padding;
end;

function TWinControlGuidelines.GetCount: Integer;
begin
  Result := inherited GetCount;
  Inc(Result, FCountDelta);
end;

function TWinControlGuidelines.GetDesignerGuideOffset(Index: Integer): Integer;
var
  Control: TWinControl;
  LRect: TRect;
  LPos: TPoint;
  LPadding: TPadding;
begin
  Control := TWinControl(Component);
  if (FCountDelta > 0) and (Index > 7) and (Index < 12) then
  begin
    LRect := Control.ClientRect;
    LPos := Control.BoundsRect.TopLeft;
    LPadding := GetControlPadding;
    if (Component <> Container) and (Control.Parent <> nil) then
    begin
      MapWindowPoints(Control.Handle, TWinControl(Container).Handle, LRect, 2);
      MapWindowPoints(Control.Parent.Handle, TWinControl(Container).Handle, LPos, 1);
    end;
    case Index of
      8: Result := LRect.Left + LPadding.Left - LPos.X;
      9: Result := LRect.Top + LPadding.Top - LPos.Y;
      10: Result := LRect.Right - LPadding.Right - LPos.X;
      11: Result := LRect.Bottom - LPadding.Bottom - LPos.Y;
    else
      Result := 0;
    end;
  end else
    Result := inherited GetDesignerGuideOffset(Index);
end;

// Since these guide lines are *internal* to the control, we need to reverse their meanings
// so that they will match correctly.

function TWinControlGuidelines.GetDesignerGuideType(Index: Integer): TDesignerGuideType;
begin
  if (FCountDelta > 0) and (Index > 7) and (Index < 12) then
    case Index of
      8: Result := gtPaddingLeft;
      9: Result := gtPaddingTop;
      10: Result := gtPaddingRight;
      11: Result := gtPaddingBottom;
    else
      Result := gtPaddingLeft;
    end
  else
    Result := inherited GetDesignerGuideType(Index);
end;

procedure TWinControlGuidelines.Initialize(AComponent, AContainer: TComponent);
begin
  inherited Initialize(AComponent, AContainer);
  if (csAcceptsControls in TWinControl(AComponent).ControlStyle) or (AComponent is TCustomFrame) then
    FCountDelta := 4;
end;

{ TCustomFormGuidelindes }

function TCustomFormGuidelines.GetControlPadding: TPadding;
var
  Form: TWinControl;
begin
  Form := TWinControl(Component);
  if (Form.Padding.Left = 0) and (Form.Padding.Top = 0) and (Form.Padding.Right = 0) and (Form.Padding.Bottom = 0) then
  begin
    if FCustomPadding = nil then
    begin
      FCustomPadding := TPadding.Create(Form);
      FCustomPadding.SetBounds(5, 5, 5, 5);
    end;
    Result := FCustomPadding;
    Exit;
  end;
  Result := Form.Padding;
end;

destructor TCustomFormGuidelines.Destroy;
begin
  FCustomPadding.Free;
  inherited;
end;

{ TActionResourceCache }

class constructor TActionResourceCache.Create;
begin
  FCache := TResourceCache.Create;
end;

class destructor TActionResourceCache.Destroy;
begin
  Clear; // Free stored items
  FreeAndNil(FCache);
end;

class procedure TActionResourceCache.Add(ComponentClass: TComponentClass; Instance: TComponent);
begin
  FCache.Add(ComponentClass, Instance);
end;

class procedure TActionResourceCache.Clear;
var
  P: TPair<TComponentClass, TComponent>;
begin
  for P in FCache do
    if P.Value <> nil then
      P.Value.Free;
  FCache.Clear;
end;

class function TActionResourceCache.GetInstance(ComponentClass: TComponentClass): TComponent;
begin
  if FCache.ContainsKey(ComponentClass) then
    Result := FCache.Items[ComponentClass]
  else
    Result := nil;
end;

class procedure TActionResourceCache.Remove(ComponentClass: TComponentClass);
begin
  if FCache.ContainsKey(ComponentClass) then
  begin
    if FCache.Items[ComponentClass] <> nil then
      FCache.Items[ComponentClass].Free;
    FCache.Remove(ComponentClass);
  end;
end;

{ TActionListView }

constructor TActionListView.Create(AOwner: TComponent);
begin
  inherited;
  FImageList := TImageList.Create(nil);
  FNewActnPopupMenu := TPopupMenu.Create(nil);
  FNewStdActnPopupMenu := TPopupMenu.Create(nil);
  FTempStringList := TStringList.Create;
  BorderStyle := bsNone;
  Columns.Add;
  Height := FDefItemHeight;
  ReadOnly := True;
  RowSelect := True;
  ShowColumnHeaders := False;
  SmallImages := FImageList;
  ViewStyle := vsReport;
  Width := 200;
end;

destructor TActionListView.Destroy;
begin
  FreeAndNil(FImageList);
  FreeAndNil(FNewActnPopupMenu);
  FreeAndNil(FNewStdActnPopupMenu);
  FreeAndnil(FTempStringList);
  inherited;
end;

procedure TActionListView.AddStdAction(const Category: string;
  ActionClass: TBasicActionClass; Info: Pointer);
var
  I: Integer;
  LCategory: string;
  List: TList<TBasicActionClass>;
begin
  if Category <> '' then
    LCategory := Category
  else
    LCategory := SActionCategoryNone;

  I := FStdActionList.IndexOf(LCategory);
  if I = -1 then
  begin
    List := TList<TBasicActionClass>.Create;
    List.Add(ActionClass);
    FStdActionList.AddObject(LCategory, List);
  end
  else
  begin
    List := TList<TBasicActionClass>(FStdActionList.Objects[I]);
    List.Add(ActionClass);
  end;
end;

procedure TActionListView.AddTempString(const S: string);
begin
  FTempStringList.Add(S);
end;

procedure TActionListView.Click;
var
  P: TPoint;
  Item: TListItem;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  Item := GetItemAt(P.X, P.Y);

  if Item <> nil then
  begin
    if Item.Index <= 1 then
    begin
      if (Item.Index = 0) and (FNewActnPopupMenu.Items.Count = 0) then
      begin
        if Assigned(FOnNewAction) then
          FOnNewAction(Self, '', TAction, FActionList);
      end
      else
      begin
        if Item.Index = 0 then
          ShowPopupMenu(Item, FNewActnPopupMenu)
        else
          ShowPopupMenu(Item, FNewStdActnPopupMenu);
      end;
    end
    else
      if (Item.Index <> 2) and Assigned(FOnSelectAction) then
        FOnSelectAction(Self, TCustomAction(Item.Data));
  end
  else
    if Assigned(FOnSelectAction) then
      FOnSelectAction(Self, nil);
end;

function TActionListView.CreateMenuItem(const Caption: string;
  Event: TNotifyEvent; CustomData: Pointer): TMenuItem;
begin
  Result := NewItem(Caption, 0, False, True, Event, 0, '');
  Result.Tag := Integer(CustomData);
end;

procedure TActionListView.CreateWnd;
begin
  inherited;
  if Designer.Root <> nil then
    RebuildListView;
end;

function TActionListView.CustomDrawItem(Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
var
  LRect: TRect;
begin
  Result := True;
  Canvas.Brush.Style := bsClear;
  LRect := Item.DisplayRect(drLabel);

  case Stage of
    cdPrePaint:
      // Draw separator
      if Item.Index = 2 then
      begin
        Canvas.Pen.Color := clSilver;
        Canvas.MoveTo(LRect.Left, LRect.Top + (LRect.Bottom - LRect.Top) div 2);
        Canvas.LineTo(LRect.Right - LRect.Left, LRect.Top + (LRect.Bottom - LRect.Top) div 2);
        Result := False; // Prevent default drawing of highlight bar
      end;
    cdPostPaint:
      // Draw arrow for New Action and New Standard Action items
      if ((Item.Index <= 1) and (FNewStdActnPopupMenu.Items.Count > 1)) and
         (((Item.Index = 0) and (FNewActnPopupMenu.Items.Count > 1)) or
         ((Item.Index = 1) and (FNewStdActnPopupMenu.Items.Count > 1))) then
      begin
        LRect.Left := LRect.Right - 20;
        if ThemeServices.ThemesEnabled and (Win32MajorVersion >= 6) then
          DrawThemeBackground(ThemeServices.Theme[teMenu], Canvas.Handle,
            MENU_POPUPSUBMENU, MSM_NORMAL, LRect, nil)
        else
          DrawArrow(Canvas, sdRight, Point(LRect.Right - 15,
            LRect.Top + ((LRect.Bottom - LRect.Top - 8) div 2)), 4);
      end;
  end;
end;

procedure TActionListView.DoNewActionClick(Sender: TObject);
begin
  if Assigned(FOnNewAction) and (Sender is TMenuItem) then
    FOnNewAction(Self, '', TAction, TCustomActionList(TMenuItem(Sender).Tag));
end;

procedure TActionListView.DoNewStdActionClick(Sender: TObject);
begin
  if Assigned(FOnNewAction) and (Sender is TMenuItem) then
    FOnNewAction(Self, '', TContainedActionClass(TMenuItem(Sender).Tag),
      TCustomActionList(TMenuItem(Sender).Parent.Tag));
end;

function TActionListView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := (Stage = cdPrePaint) or (Stage = cdPostPaint);
end;

procedure TActionListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if Assigned(FOnSelectAction) and (Selected <> nil) then
      begin
        if (Selected.Index = 0) and (FActionList <> nil) and Assigned(FOnNewAction) then
          FOnNewAction(Self, '', TAction, FActionList)
        else if Selected.Index = 0 then
          ShowPopupMenu(Selected, FNewActnPopupMenu)
        else if Selected.Index = 1 then
          ShowPopupMenu(Selected, FNewStdActnPopupMenu)
        else if Selected.Index <> 2 then
          FOnSelectAction(Self, TCustomAction(Selected.Data));
      end;
    VK_RIGHT:
      if Selected <> nil then
      begin
        if Selected.Index = 0 then
          ShowPopupMenu(Selected, FNewActnPopupMenu)
        else
          ShowPopupMenu(Selected, FNewStdActnPopupMenu);
      end;
  else
    inherited;
  end;
end;

procedure TActionListView.RebuildListView;
var
  LRect: TRect;
  LIcon: TIcon;
  ListItem: TListItem;
  LAction: TCustomAction;
  I, LWidth, MinWidth: Integer;
begin
  // Add actions to listview
  if FNewStdActnPopupMenu.Items.Count > 0 then
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      FImageList.Clear;

      // Set initial max width
      MinWidth := Max(Width, Canvas.TextWidth(SCreateNewStdAction) + 25);

      // Find all actions
      FTempStringList.Clear;
      Designer.GetComponentNames(GetTypeData(TypeInfo(TCustomAction)), AddTempString);
      for I := 0 to FTempStringList.Count - 1 do
      begin
        LAction := TCustomAction(Designer.GetComponent(FTempStringList[I]));
        ListItem := Items.Add;
        ListItem.Caption := FTempStringList[I];
        ListItem.Data := LAction;
        ListItem.ImageIndex := -1;
        LWidth := Canvas.TextWidth(ListItem.Caption);
        if (LWidth + 40) > MinWidth then
          MinWidth := LWidth + 40;

        if (LAction.ImageIndex <> -1) and (LAction.ActionList <> nil) and
           (LAction.ActionList.Images <> nil) then
        begin
          LIcon := TIcon.Create;
          try
            LAction.ActionList.Images.GetIcon(LAction.ImageIndex, LIcon);
            ListItem.ImageIndex := FImageList.AddIcon(LIcon);
          finally
            LIcon.Free;
          end;
        end;
      end;

      // Sort list items before adding "special" items
      CustomSort(nil, 0);

      // Add "New Action" item
      ListItem := Items.Insert(0);
      ListItem.Caption := SCreateNewAction;
      ListItem.ImageIndex := -1;

      // Add "New Standard Action" item
      ListItem := Items.Insert(1);
      ListItem.Caption := SCreateNewStdAction;
      ListItem.ImageIndex := -1;

      // Add dummy item for divider line
      if Items.Count > 2 then
      begin
        ListItem := Items.Insert(2);
        ListItem.ImageIndex := -1;
      end;

    finally
      Items.EndUpdate;
    end;

    // Set Height to fit 14 items
    LWidth := 0;
    if Items.Count > 14 then
    begin
      I := 14;
      LWidth := GetSystemMetrics(SM_CXVSCROLL);
    end
    else
      I := Items.Count;
    LRect := Items[0].DisplayRect(drBounds);
    Height := LRect.Bottom * I;

    // Set width to widest + space for icon and gutters (20 pixels each side)
    Width := MinWidth + LWidth + FImageList.Width;
    Columns[0].Width := Width - LWidth;
  end
  else
    Height := FDefItemHeight;
end;

procedure TActionListView.RebuildPopupMenus;
var
  LName: string;
  J, I, K: Integer;
  MenuItem, SubItem: TMenuItem;
  LActionList: TCustomActionList;
  LActionLists: TList<TCustomActionList>;
  ActionClassList: TList<TBasicActionClass>;
begin
  SmallImages := nil;
  MenuItem := nil;

  // Build popup menus
  FNewActnPopupMenu.Items.Clear;
  FNewStdActnPopupMenu.Items.Clear;
  FStdActionList := TStringList.Create;
  try
    // Gather list of registered action classes
    if Assigned(EnumRegisteredActionsProc) then
      EnumRegisteredActions(AddStdAction, nil);

    LActionLists := TList<TCustomActionList>.Create;
    try
      // Build list of ActionLists
      FTempStringList.Clear;
      Designer.GetComponentNames(GetTypeData(TypeInfo(TCustomActionList)), AddTempString);
      for I := 0 to FTempStringList.Count - 1 do
        LActionLists.Add(TCustomActionList(Designer.GetComponent(FTempStringList[I])));

      // If there's just one, save it in FActionList
      if LActionLists.Count = 1 then
        FActionList := LActionLists[0]
      else
        FActionList := nil;

      // Build popupmenus for actionlists and standard actions
      for LActionList in LActionLists do
      begin
        // Build a popup menu for each ActionList
        if LActionList.Images <> nil then
          SmallImages := FImageList;

        // If more than 1 actionlist, add a popupmenu to select
        // where the create the action
        if LActionLists.Count > 1 then
        begin
          if LActionList.Owner = Designer.Root then
            LName := LActionList.Name
          else
            LName := LActionList.Owner.Name + DotSep + LActionList.Name;

          MenuItem := CreateMenuItem(LName, DoNewActionClick, LActionList);
          FNewActnPopupMenu.Items.Add(MenuItem);
          MenuItem := CreateMenuItem(LName, nil, nil);
          FNewStdActnPopupMenu.Items.Add(MenuItem);
        end;

        // For standard actions popup, add each standard action category
        for J := 0 to FStdActionList.Count - 1 do
        begin
          SubItem := CreateMenuItem(FStdActionList[J], nil, LActionList);
          if LActionLists.Count > 1 then
            MenuItem.Add(SubItem)
          else
            FNewStdActnPopupMenu.Items.Add(SubItem);

          // For each category, add each registered action class
          ActionClassList := TList<TBasicActionClass>(FStdActionList.Objects[J]);
          for K := 0 to ActionClassList.Count - 1 do
            SubItem.Add(CreateMenuItem(ActionClassList[K].ClassName, DoNewStdActionClick, ActionClassList[K]));
        end;
      end;
    finally
      LActionLists.Free;
    end;

    // Free lists of registered action classes
    for I := 0 to FStdActionList.Count - 1 do
      FStdActionList.Objects[I].Free;
  finally
    FreeAndNil(FStdActionList);
  end;
end;

procedure TActionListView.SetDesigner(const Value: IDesigner);
begin
  if Value <> FDesigner then
  begin
    FDesigner := Value;

    // Set initial height based on default item height
    FTempStringList.Clear;
    Designer.GetComponentNames(GetTypeData(TypeInfo(TCustomAction)), AddTempString);
    if FTempStringList.Count > 0 then
      Height := (Min(FTempStringList.Count, 11) + 3) * FDefItemHeight
    else
      Height := FDefItemHeight;

    // Rebuild popup menus and listview
    RebuildPopupMenus;
    if HandleAllocated then
      RebuildListView;
  end;
end;

procedure TActionListView.ShowPopupMenu(Item: TListItem; PopupMenu: TPopupMenu);
var
  P: TPoint;
  LRect: TRect;
begin
  LRect := Item.DisplayRect(drBounds);
  P := Item.Owner.Owner.ClientToScreen(Point(LRect.Right, LRect.Top));
  PopupMenu.Tag := Integer(Item.Data);
  PopupMenu.Popup(P.X, P.Y);
end;

{ TActionProperty }

destructor TActionProperty.Destroy;
begin
  if FActionListView <> nil then
    FreeAndNil(FActionListView);
  inherited;
end;

procedure TActionProperty.CreateNewAction(Sender: TObject; const Category: string;
  ActionClass: TContainedActionClass; ActionList: TCustomActionList);
var
  LRoot: IRoot;
  LCategory: string;
  Image, Mask: TBitmap;
  NewAction: TContainedAction;
begin
  LCategory := Category;
  if AnsiCompareText(LCategory, SActionCategoryNone) = 0 then
    LCategory := '';

  // Create new action
  NewAction := CreateAction(ActionList.Owner, ActionClass) as TContainedAction;
  try
    if ActionList.Owner = Designer.Root then
      NewAction.Name := Designer.UniqueName(ActionClass.ClassName)
    else
    begin
      LRoot := ActiveDesigner.FindRoot(ActionList.Owner);
      if LRoot <> nil then
        NewAction.Name := LRoot.GetDesigner.UniqueName(ActionClass.ClassName)
      else
        raise Exception.CreateResFmt(@SUnableToFindComponent, [ActionList.Owner.Name]);
    end;
    NewAction.Category := LCategory;
    NewAction.ActionList := ActionList;

    if ActionList.Images <> nil then
    begin
      // Use BeginUpdate to prevent the ImageList's ChangeLink from causing the
      // Object Inspector to refresh, freeing the property editor in the process
      ActionList.Images.BeginUpdate;
      try
        // Copy image if available
        if (NewAction is TCustomAction) and (THackAction(NewAction).FImage <> nil) and
           (ActionList.Images.Height = 16) and (ActionList.Images.Width = 16) then
        begin
          Image := TBitmap.Create;
          try
            Mask := TBitmap.Create;
            try
              Image.SetSize(16, 16);
              Mask.SetSize(16, 16);

              Image.Canvas.Draw(0, 0, TBitmap(THackAction(NewAction).FImage));
              Mask.Canvas.Draw(0, 0, TBitmap(THackAction(NewAction).FMask));

              Image.PixelFormat := pf8bit;
              Mask.PixelFormat := pf8bit;

              THackAction(NewAction).FImage.Free;
              THackAction(NewAction).FMask.Free;
              THackAction(NewAction).FImage := nil;
              THackAction(NewAction).FMask := nil;
              THackAction(NewAction).ImageIndex := ActionList.Images.Add(Image, Mask);
            finally
              Mask.Free;
            end;
          finally
            Image.Free;
          end;
        end;

        // Update property
        SelectAction(Sender, NewAction);
      finally
        ActionList.Images.EndUpdate;
      end;
    end
    else
      // Update property
      SelectAction(Sender, NewAction);
  except
    NewAction.Free;
    raise;
  end;
end;

procedure TActionProperty.SelectAction(Sender: TObject; Action: TContainedAction);
begin
  FHost.CloseDropDown;
  if Action <> nil then
    SetValue(Action.Owner.Name + DotSep + Action.Name)
  else
    SetValue('');
end;

procedure TActionProperty.Edit(const Host: IPropertyHost; DblClick: Boolean);
var
  LHost20: IPropertyHost20;
begin
  FHost := Host;
  if FActionListView <> nil then
    FActionListView.Free;

  FActionListView := TActionListView.Create(nil);
  if Supports(FHost, IPropertyHost20, LHost20) then
    FActionListView.Width := LHost20.GetDropDownWidth;
  FActionListView.OnNewAction := CreateNewAction;
  FActionListView.OnSelectAction := SelectAction;
  FActionListView.Designer := Designer;
  FActionListView.Visible := True;
  FHost.DropDownControl(FActionListView);
end;

function TActionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited + [paCustomDropDown, paVolatileSubProperties] - [paValueList, paSortList];
end;

initialization
  CF_COMPONENTS := RegisterClipboardFormat(cfDelphiComponents);
  CF_COMPONENT := RegisterClipboardFormat(cfDelphiComponent);
  NotifyGroupChange(UnregisterActionGroup);

finalization
  UnNotifyGroupChange(UnregisterActionGroup);

end.
