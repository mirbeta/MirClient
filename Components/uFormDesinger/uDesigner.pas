unit uDesigner;
//本单元界面设计
interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, ComObj, ActiveX,  Menus,
  Graphics, uNonVisibleComponentDesgn, uDesignIntf, uDesignMenus,
  TypInfo, IniFiles, uDesignEditors, uPropertyManager, uToolsAPI;

type
  TCustomDesigner = class;
  TCrackComponent = class(TComponent);
  TCrackControl = class(TControl);
  //对齐
  TAlignAffect  = (alTop{顶部对齐}, alLeft{左对齐}, alBottom{底对齐}, alRight{右对齐}, alHCenter{水平居中}, alVCenter{垂直居中},
    alHSpace{水平间距相等}, alVSpace{垂直间距相等}, alHWinCenter{置于窗口水平中央}, alVWinCenter{置于窗口垂直中央},
    alHSpaceInc{增加水平间距}, alHSpaceDec{缩小水平间距}, alHSpaceDel{移除水平间距}, alVSpaceInc{增加垂直间距},
    alVSpaceDec{缩小垂直间距}, alVSpaceDel{删除垂直间距}, alAlignToGrid{对齐到表格}, alSnapToGrid{吸附到表格},
    alSendToBack{置后}, alBringToFront{置前});
  TSizeAffect  = (saNothing{无变化}, saHGrow{增加到最大宽度}, saHShrink{缩小到最小宽度}, saHAbsolute{指定宽度},
                saVGrow{增加到最大高度}, saVShrink{缩小到最小高度}, saVAbsolute{指定高度},
                saWidths{相同宽度}, saHeights{相同高度}, saWidthHeight{大小完全相同}, saSizeToGrid{四边对齐到表格的大小});

  IComponentsMonitor  = interface
    ['{0F210B78-6BD5-44F5-802C-3FEB39826475}']
    procedure AddComponent(Component: TComponent);
    procedure DeleteComponent(Component: TComponent);
    procedure ActiveComponent(Component: TComponent);
  end;

  THandlePointManager = class(TComponent)
  private
    FDesigner: TCustomDesigner;
  public
    constructor Create(ADesigner: TCustomDesigner);
  end;

  TNVCHandleManager = class(TComponent)   //管理非可视组件显示
  private
    FDesigner: TCustomDesigner;
  public
    constructor Create(ADesigner: TCustomDesigner);
    procedure UpdateComponentName(Component: TComponent);
  end;

  TRDComponentEditor      = class(TComponentEditor);
  TRDComponentEditorClass = class of TRDComponentEditor;
  TRDPropertyEditor       = class(TPropertyEditor);
  TRDPropertyEditorClass  = class of TRDPropertyEditor;

  ///记录当前IDE选择的那些东西
  TRDesignderSelections = class(TInterfacedObject, IDesignerSelections)
  private
    FList: TList;
  protected
    function Add(const Item: TPersistent): Integer;
    function Equals(const List: IDesignerSelections): Boolean; reintroduce;
    function Get(Index: Integer): TPersistent;
    function GetDesignObject(Index: Integer): IDesignObject;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read Get; default;
  public
    constructor Create; virtual;
    constructor Copy(const Selections: IDesignerSelections);
    destructor Destroy; override;

    function IndexOf(const Value: TPersistent): Integer;
    procedure Delete(Item: TPersistent); overload;
    procedure Delete(const Index: Integer); overload;
    procedure ClearSelections;
  end;

  TOnNonVisbleComponentCreate = procedure (const AComponent: TComponent; var CanShow: boolean) of object;
  TOnBeforeDelete = procedure(Sender: TObject; Deleted: TObject) of Object;
  TOnGetMethodName  = procedure(const Method: TMethod; var MethodName: String) of Object;
  TOnFindMethod = procedure(const MethodName: String; var Found: boolean) of Object;
  TOnGetConfigFolder  = procedure(var Folder: String) of Object;
  TOnComponentNameChanged = procedure(Component: TComponent) of Object;
  TOnCreateMethod = procedure(const Name: string; TypeData: PTypeData; var Method: TMethod) of Object;
  TOnCreateMethodX  = procedure(const Name: string; const AEventInfo: IEventInfo; var Method: TMethod) of Object;
  TOnGetMethodsX = procedure(const AEventInfo: IEventInfo; Proc: TGetStrProc) of Object;
  TOnShowMethod = procedure(const Name: String) of Object;
  TOnReNameMethod = procedure (const OldName, NewName: String) of Object;
  TOnGetMethods = procedure(TypeData: PTypeData; Proc: TGetStrProc) of Object;
  TOnMethodFromAncestor = procedure(const Method: TMethod; var Value: Boolean) of Object;

  TCustomDesigner = class(TComponent, IDesignerHook, IDesigner)
  private
    FModified: boolean;
    FComponentsMonitor: TList;
    FSelection: TRDesignderSelections;
    FOnNonVisbleComponentCreate: TOnNonVisbleComponentCreate;
    FOnBeforeDelete: TOnBeforeDelete;
    FOnAfterAddComponent, //TNotifyEvent中间sender参数代表新增组件
    FOnBeginDesign,
    FOnQuitDesign: TNotifyEvent;
    FOnModified,
    FOnAfterPaste: TNotifyEvent;
    FOnElementSelected: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FDraggedList: TList;
    FLocked: boolean; //是否锁定
    FGridSize: integer;//点阵大小
    FSnapToGrid: Boolean;
    FDesingerPointSize: integer;  //设计八个点的大小
    FGridColor, //窗体点阵颜色
    FSingleSelectedColor,
    FMutilSelectedColor: TColor; //八点的颜色
    FShowGrid: boolean;  //是否显示点阵
    FGrabHandleManager: THandlePointManager;
    FNVCHandleManager: TNVCHandleManager;
    FDesignForm: TCustomForm;
    FDraggingControl: TControl;
    FDragging: Boolean;
    FBeforDragPos: TPoint;
    FPopupEditors: TObject;//TPopupEditors;
    FShowPopupEditor: boolean;  //是否显示菜单进行组件设计
    FSelecting: Boolean;
    FPointStart, FPointEnd: TPoint;
    FOldRect: TRect;
    FNewRect: TRect;
    FCreateMethod: TOnCreateMethod;
    FCreateMethodX: TOnCreateMethodX;
    FOnGetMethodsX: TOnGetMethodsX;
    FGetMethodName: TOnGetMethodName;
    FOnFindMethod: TOnFindMethod;
    FOnShowMethod: TOnShowMethod;
    FOnReNameMethod: TOnReNameMethod;
    FOnGetMethods: TOnGetMethods;
    FOnFormSizeChange: TNotifyEvent;
    FMethodFromAncestor: TOnMethodFromAncestor;
    FPropManager: TuPropertiesManager;

    FMouseRect: TRect; //鼠标被限制的范围
//    FOnGetVisbleComponentICO: TOnGetVisbleComponentICO;
    FOnGetConfigFolder: TOnGetConfigFolder;
    FOnComponentNameChanged: TOnComponentNameChanged;
    FSelectionChange: TNotifyEvent;
    FAllowFormPostionChange: boolean;
    FConfigFolder: String;
    {***MouseLock:
    *鼠标位置锁定,在窗体上按鼠标不放时,只允许鼠标的范围在
    *Sender.parent范围内,或者容易发生焦点的改变
    *Sender代表当前选中的控件
    **}
    procedure MouseLock(Sender: TControl);//锁定鼠标到某一个范围
    {**MouseFree:
    *释放锁定的鼠标
    **}
    procedure MouseFree();//释放对鼠标的锁定
    {**OnMessage
    *Sender:当前选中的控件
    *捕获发生在sender上面的消息
    **}
    function OnMessage(Sender: TControl; var Message: TMessage): Boolean;
    {**Remove(AControl: TControl):
    * 将AControl从当前设计列表中移除
    **}
    procedure Remove(AControl: TControl); overload;
    {**Remove(Index: Integer)
    *将序号为Index的控件从当前设计列表中移除
    **}
    procedure Remove(Index: Integer); overload;
    {**ClearSelections()
    *清楚所有设计控件,清除后不会显示设计的八个小点
    **}
    procedure ClearSelections();
    {**Add(AControl: TControl)
    *将控件AControl增加到设计列表,显示设计的八个小点
    **}
//    function Add(AControl: TControl): TControl;
    {
      增加一个选择项
    }
    procedure AddSelected(Selected: TPersistent);
    {**ClearGrabHandle(AControl: TControl)
    *销毁AControl上面的设计的八个小点
    **}
    procedure ClearGrabHandle(AControl: TControl);
    {**SetDragging(const Value: Boolean)
    *指定是否处于拖拉状态,八个设计小点的显示与参数取反
    **}
    procedure SetDragging(const Value: Boolean);
    {**GetSelectionCount:
    *获取当前设计列表的控件总数
    **}
    function GetSelectionCount: Integer;
    {**GetControls(Index: Integer):
    *获取设计列表中序号为Index的控件,无法取得正确的非可视组件
    *故本函数不对外,只在内部使用
    **}
    function GetControls(Index: Integer): TControl;
    {**GetSelection(Index: Integer):
    *由于GetControls(i)取不到正确的非可视组件,所以本函数用来取出正确的
    *组件,对外开放用来取得组件
    **}
    function GetSelection(Index: Integer): TPersistent;
    {**
    *GetComponentFromSelect:从当前选择的控件获取真正的那个控件
    **}
    function GetComponentFromSelect(Selected: TControl): TComponent;
    {**AddRectControls(Parent: TWinControl; Rect: TRect):
    *在parent之上的区域rect范围内的控件全部添加到设计列表
    **}
    procedure AddRectControls(Parent: TWinControl; Rect: TRect);
    {**OwnerCheck(Sender: TControl; CheckOnwer: TComponent):
    * 查看sender的拥有者是否就是CheckOnwer
    **}
    function OwnerCheck(Sender: TControl; CheckOnwer: TComponent): Boolean;
    {**MouseDown:
    *鼠标按下处理工程,选择设计控件\增加新组件\弹出菜单
    **}
    procedure MouseDown(Sender: TControl; Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); virtual;
    {**MouseUp:
    *处理释放锁定的鼠标和区域性选择
    **}
    procedure MouseUp(Sender: TControl; Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); virtual;
    {**MouseMove:
    *处理鼠标移动,多选时画框,改变组件大小等
    **}
    procedure MouseMove(Sender: TControl; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure KeyDown(Sender: TControl; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyPress(Sender: TControl; var Message: TWMKey);
    {**RemoveFromForm:
    *将一个组件从窗体移除,如果是非可视组件还负责移除外在的设计控件
    **}
    procedure RemoveFromForm(c: TComponent);
    {**BuildDesignNonVisbleComponent:
    *为指定的非可视组件处理外在的显示
    **}
    procedure BuildDesignNonVisbleComponent(AComponent: TComponent);
    {**setDesignComponent:
    *指定当前的设计组件
    **}
    procedure setDesignComponent(value: TComponent);
    {**getDesignComponent:
    *取得当前正在设计的组件
    **}
    function getDesignComponent: TComponent;
    {**getCanCopyCutOrDelete:
    *是否可以复制剪切和删除,以是否选中了控件为依据
    **}
    function getCanCopyCutOrDelete: boolean;
    {**getCanPaste:
    *是否可以粘贴,判断用于复制粘贴的流的大小是否大于0
    **}
    function getCanPaste: boolean;
    {**NonCompBringFront:
    *将所有非可视组件的外在表现控件放到最前端
    **}
    procedure NonCompBringFront;
    {**ControlInSelection:
    *查看组件是否正在被设计
    **}
    function ControlInSelection(Sender: TControl): boolean;
    {**getControlByNVCHandleManager:
    *根据一非可视组件查看其外在的表现控件
    **}
    function GetControlByNVCHandleManager(Comp: TComponent): TControl;
    {**getControlByEliminateAssembled:
    *排除组合控件
    **}
    function GetControlByEliminateAssembled(c: TControl): TControl;
    ///
    ///  ObjISNonVisibleComponent:组件是否非可视
    function ObjISNonVisibleComponent(Obj: TPersistent): boolean;
    //当前选择了多少个对象
    function GetSelectCount: Integer;
    function GetSelected(Index: Integer): TPersistent;

    procedure SetPropManager(const Value: TuPropertiesManager);

    function CreateSelection: TRDesignderSelections;
    procedure DoModified;//处理变更

    function IsDesignControl(Control: TControl): Boolean;
    procedure UpdateDrag;  //修正牵引位置
    function PaintControl(Sender: TControl; var Message: TMessage): Boolean;
    procedure PaintDesignGrid(DC: HDC; Ctrl: TControl);

    procedure ReaderSetName(Reader: TReader; Component: TComponent; var Name: string);
    procedure ComponentRead(Component: TComponent);
    procedure EditComponent(Component: TComponent);
    function GetParent: TWinControl;
    procedure MonitorAddCompoent(Component: TComponent);
    procedure MonitorDeleteComponent(Component: TComponent);
    procedure MonitorActiveComponent(Component: TComponent);
  protected
    {**ShowComponentEditor:
    *处理菜单编辑器
    **}
    procedure ShowComponentEditor;
    { IDesignerNotify 的接口}
    procedure Modified;
    procedure DesignerNotification(AnObject: TPersistent; Operation: TOperation);
    procedure IDesignerHook.Notification=DesignerNotification;
    procedure CanInsertComponent(AComponent: TComponent);


    { IDesignerHook 的接口}
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    {**PaintGrid:
    *画窗体上的表格
    **}
    procedure PaintGrid;
    procedure PaintMenu;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string);
    function GetRoot: TComponent;

    {IDesigner}
    function GetAppDataDirectory(Local: Boolean = False): string;
    {IDesigner100}
    function GetDesignerExtension: string;
    property DesignerExtention: string read GetDesignerExtension;
    {IDesigner80}
    function CreateMethod(const Name: string; const AEventInfo: IEventInfo): TMethod; overload;
    procedure GetMethods(const AEventInfo: IEventInfo; Proc: TGetStrProc); overload;
    procedure SelectComponent(const ADesignObject: IDesignObject); overload;
    {IDesigner70}
    function GetActiveClassGroup: TPersistentClass;
    function FindRootAncestor(const AClassName: string): TComponent;
    property ActiveClassGroup: TPersistentClass read GetActiveClassGroup;
    {IDesigner60}
    procedure Activate;
    procedure IDesigner.Modified=Designer60Modified;
    procedure Designer60Modified;
    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod; overload;
    function GetMethodName(const Method: TMethod): string;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc); overload;
    function GetPathAndBaseExeName: string;
    function GetPrivateDirectory: string;
    function GetBaseRegKey: string;
    function GetIDEOptions: TCustomIniFile;
    procedure GetSelections(const List: IDesignerSelections);
    function MethodExists(const Name: string): Boolean;
    procedure RenameMethod(const CurName, NewName: string);
    procedure SelectComponent(Instance: TPersistent); overload;
    procedure SetSelections(const List: IDesignerSelections);
    procedure ShowMethod(const Name: string);
    procedure GetComponentNames(TypeData: PTypeData; Proc: TGetStrProc);
    function GetComponent(const Name: string): TComponent; overload;
    function GetComponentName(Component: TComponent): string;
    function GetObject(const Name: string): TPersistent;
    function GetObjectName(Instance: TPersistent): string;
    procedure GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc);
    function MethodFromAncestor(const Method: TMethod): Boolean;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    function CreateCurrentComponent(Parent: TComponent; const Rect: TRect): TComponent;
    function IsComponentLinkable(Component: TComponent): Boolean;
    function IsComponentHidden(Component: TComponent): Boolean;
    procedure MakeComponentLinkable(Component: TComponent);
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
    function GetIsDormant: Boolean;
    procedure GetProjectModules(Proc: TGetModuleProc);
    function GetAncestorDesigner: IDesigner;
    function IsSourceReadOnly: Boolean;
    function GetScrollRanges(const ScrollPosition: TPoint): TPoint;
    procedure Edit(const Component: TComponent);
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      TypeData: PTypeData); overload;
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      const AEventInfo: IEventInfo); overload;
    procedure CopySelection;
    procedure CutSelection;
    function CanPaste: Boolean;
    procedure PasteSelection;
    procedure DeleteSelection(ADoAll: Boolean = False);
    procedure ClearSelection;
    procedure NoSelection;
    procedure ModuleFileNames(var ImplFileName, IntfFileName, FormFileName: string);
    function GetRootClassName: string;
    function IDesigner.UniqueName=DesignerUniqueName;
    function DesignerUniqueName(const BaseName: string): string;
    function IDesigner.GetRoot=Designer60GetRoot;
    function Designer60GetRoot: TComponent;
    function GetShiftState: TShiftState;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);
    procedure SelectItemName(const PropertyName: string);
    procedure Resurrect;
    //组件本身的Notification
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Root: TComponent read GetRoot;
    property IsDormant: Boolean read GetIsDormant;
    property AncestorDesigner: IDesigner read GetAncestorDesigner;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Dragging: Boolean read FDragging write SetDragging;
    property Selected[Index: Integer]: TPersistent read GetSelected;
    property SelectCount: Integer read GetSelectCount;
    property ISModified: boolean read FModified;
    property ConfigFolder: String read FConfigFolder write FConfigFolder;
    property PropertiesManager: TuPropertiesManager read FPropManager write SetPropManager;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnGetConfigFolder: TOnGetConfigFolder read FOnGetConfigFolder write FOnGetConfigFolder;
    property OnComponentNameChanged: TOnComponentNameChanged read FOnComponentNameChanged write FOnComponentNameChanged;
    property OnSelectionChange: TNotifyEvent read FSelectionChange write FSelectionChange;
  public
    constructor Create(Onwer: TComponent);override;
    destructor Destroy; override;
    {**UniqueName
    *获取一个组件的唯一名称
    **}
    function UniqueName(const BaseName: string): string;
    {**AddComponentForm:
    *增加组件到窗体,负责处理非可视组件的显示
    **}
    procedure InsertNewComponent(CompnentClass: TComponentClass);
    {**CompelOnAfterAddComponent
    *强制提示已经增加新组件,非直接在窗体上增加的方式调用.比如增加tabsheet
    **}
    procedure CompelOnAfterAddComponent(Adder: TObject);
    {**ShowGrabHandle:
    *显示/隐藏八个小点
    **}
    procedure ShowGrabHandle(const Show: boolean);
    {**Design:
    *设计窗体
    **}
    procedure Design(OwnerForm: TCustomForm);
    {**QuitDesign:
    *退出窗体的设计
    **}
    procedure Select(Items: array of TPersistent);
    function QuitDesign: boolean;
    procedure Cut;
    procedure Copy;
    procedure Delete;
    procedure Paste;
    procedure SelectAll;
    {**Align:
    *处理对齐
    **}
    procedure Align(Affect: TAlignAffect);
    procedure Size(Affect: TSizeAffect; Value: Integer);

    {**BuildAllDesignNonVisbleComponent:
    *创建全部非可视组件的外在显示
    **}
    procedure BuildAllDesignNonVisbleComponent;  //创建非可视的组件的设计
    procedure RegisterComponentsMonitor(Monitor: IComponentsMonitor);
    procedure UnRegisterComponentsMonitor(Monitor: IComponentsMonitor);

    procedure RegisterComponentEditor(AComponentClass: TComponentClass; AEditorClass: TRDComponentEditorClass);
    procedure RegisterPropertyEditor(PropertyType: PTypeInfo; ComponentClass: TClass;
                const PropertyName: string; EditorClass: TRDPropertyEditorClass);
    property SelectionCount: Integer read GetSelectionCount;
    property Selections[Index: Integer]: TPersistent read GetSelection;
    property DesignForm: TCustomForm read GetCustomForm write SetCustomForm;
//    property DesignComponent: TComponent read getDesignComponent write setDesignComponent;
    property CanCopyElements: boolean read getCanCopyCutOrDelete;    //返回是否可以复制剪切粘贴删除
    property CanCutElements: boolean read getCanCopyCutOrDelete;
    property CanPasteElements: boolean read getCanPaste;
    property CanDeleteElements: boolean read getCanCopyCutOrDelete;
  published
    property ShowPopupEditor: boolean read FShowPopupEditor write FShowPopupEditor default true;   //是否显示菜单设计器
    property Locked: boolean read FLocked write FLocked;  //是否锁定,锁定后无法移动和改变组件大小
    property ShowGrid: boolean read FShowGrid write FShowGrid;  //是否显示窗体的表格
    property GridSize: Integer read FGridSize write FGridSize default 4;  //窗体上表格的距离
    property GridColor: TColor read FGridColor write FGridColor default clGray; //表格的颜色
    property DesingerPointSize: integer read FDesingerPointSize write FDesingerPointSize default 4; //八个设计小点的颜色
    property SingleSelectedColor: TColor read FSingleSelectedColor write FSingleSelectedColor default clBlack;//单选时八个设计小点的颜色
    property MutilSelectedColor: TColor read FMutilSelectedColor write FMutilSelectedColor default clAppWorkSpace;//多选时八个设计小点的颜色
    property AllowFormPostionChange: boolean read FAllowFormPostionChange write FAllowFormPostionChange default False;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnBeginDesign: TNotifyEvent read FOnBeginDesign write FOnBeginDesign;
    {OnQuitDesign: 退出设计}
    property OnQuitDesign: TNotifyEvent read FOnQuitDesign write FOnQuitDesign;
    {OnAfterAddComponent: 增加一个组件之后}
    property OnAfterAddComponent: TNotifyEvent read FOnAfterAddComponent write FOnAfterAddComponent;
    {OnElementSelected: 提示设计的组件已经发生改变,实际选择一组件就是鼠标单击事件产生的}
    property OnElementSelected: TNotifyEvent read FOnElementSelected write FOnElementSelected;
    {OnBeforeDelete: 在删除一个组件之后}
    property OnBeforeDelete: TOnBeforeDelete read FOnBeforeDelete write FOnBeforeDelete;
    property OnNonVisbleComponentCreate: TOnNonVisbleComponentCreate read FOnNonVisbleComponentCreate write FOnNonVisbleComponentCreate;
//    property OnGetVisbleComponentICO: TOnGetVisbleComponentICO read FOnGetVisbleComponentICO write FOnGetVisbleComponentICO;
    property OnAfterPaste: TNotifyEvent read FOnAfterPaste write FOnAfterPaste;
    property OnGetMethodName: TOnGetMethodName read FGetMethodName write FGetMethodName;
    property OnCreateMethod: TOnCreateMethod read FCreateMethod write FCreateMethod;
    property OnCreateMethodX: TOnCreateMethodX read FCreateMethodX write FCreateMethodX;
    property OnGetMethodsX: TOnGetMethodsX read FOnGetMethodsX write FOnGetMethodsX;
    property OnShowMethod: TOnShowMethod read FOnShowMethod write FOnShowMethod;
    property OnFindMethod: TOnFindMethod read FOnFindMethod write FOnFindMethod;
    property OnReNameMethod: TOnReNameMethod read FOnReNameMethod write FOnReNameMethod;
    property OnGetMethods: TOnGetMethods read FOnGetMethods write FOnGetMethods;
    property OnMethodFromAncestor: TOnMethodFromAncestor read FMethodFromAncestor write FMethodFromAncestor;
    property OnFormSizeChange: TNotifyEvent read FOnFormSizeChange write FOnFormSizeChange;
  end;

  TuFormDesigner = class(TCustomDesigner)
  public
    property SelectionCount;
    property Selections;
    property DesignForm;
//    property DesignComponent;
  published
    property ShowGrid;
    property GridSize;
    property GridColor;
    property PropertiesManager;
    property DesingerPointSize;
    property SingleSelectedColor;
    property MutilSelectedColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnBeginDesign;
    property OnQuitDesign;
    property OnNonVisbleComponentCreate;
    property OnComponentNameChanged;
    property OnModified;
    property OnSelectionChange;
  end;

implementation
  uses uComponentEditors, uPicEdit, RTLConsts, ClipBrd, uVCLEditors, uColnEdit, uPackageManager;

const
  Form_POSCHANGED = WM_USER + $1001;

type
  PDesignNotificationRec = ^TDesignNotificationRec;
  TDesignNotificationRec  = record
    DesignNotification: IDesignNotification;
  end;

  DesignNotificationManager = class(TList)
  protected
    procedure RegisterDesignNotification(const DesignNotification: IDesignNotification);
    procedure UnRegisterDesignNotification(const DesignNotification: IDesignNotification);

    procedure PostItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure PostItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure PostItemsModified(const ADesigner: IDesigner);
    procedure PostSelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure PostDesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure PostDesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  end;

  PComponentsMonitor  = ^TComponentsMonitor;
  TComponentsMonitor  = record
    ComponentsMonitor: IComponentsMonitor;
  end;

  //八点的方向
  THandlePointDirect = (hpdLeftUp, hpdUp, hpdRightUp, hpdRight,
    hpdRightDown, hpdDown, hpdLeftDown, hpdLeft);

  //八点
  THandlePoint = class(TCustomControl)
  private
    FPointSize: integer;
    FManager: THandlePointManager;
    FControl: TControl;
    FDirect: THandlePointDirect;
    FDesigner: TCustomDesigner;
    procedure WMEraseBkgnd(var Message: TWMErasebkgnd); message WM_ERASEBKGND;
    procedure Pos();
    function GeTCustomFormDesigner: TCustomDesigner;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ChangeScale(M: Integer; D: Integer); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
    property PointSize: integer read FPointSize write FPointSize;
  public
    constructor Create(AManager: TComponent; AControl: TControl; AParent: TWinControl; ADirect: THandlePointDirect);
    destructor Destroy; override;
    property Designer: TCustomDesigner read GeTCustomFormDesigner;
  end;

  TWinCtrolCrack  = class(TWinControl);

var
  DesignNotificationMgr: DesignNotificationManager;

function PointToRect(pt1, pt2: TPoint): TRect;
begin
  if pt1.X < pt2.X then
  begin
    Result.Left := pt1.X;
    Result.Right := pt2.X;
  end
  else
  begin
    Result.Left := pt2.X;
    Result.Right := pt1.X;
  end;
  if pt1.Y < pt2.Y then
  begin
    Result.Top := pt1.Y;
    Result.Bottom := pt2.Y;
  end
  else
  begin
    Result.Top := pt2.Y;
    Result.Bottom := pt1.Y;
  end;
end;

{ TCustomDesigner }
procedure TCustomDesigner.Activate;
begin
end;

procedure TCustomDesigner.AddRectControls(Parent: TWinControl; Rect: TRect);
  function InRect(R1, R2: TRect): Boolean;
  begin
    Result := False;
    if not IntersectRect(R1, R1, R2) then
      Exit;
    Result := not IsRectEmpty(R1);
  end;
var
  I: Integer;
begin
  ClearSelections();
  for I := 0 to Parent.ControlCount - 1 do
    if InRect(Rect, Parent.Controls[I].BoundsRect) and not IsDesignControl(Parent.Controls[I]) then //and
      //OwnerCheck(Parent.Controls[I], FRoot) then    //对于非可视组件的外在表现控件拥有者不是窗体
    begin
      AddSelected(Parent.Controls[I]);
    end;
end;

procedure TCustomDesigner.AddSelected(Selected: TPersistent);
var
  D: THandlePointDirect;
  FrameSize: THandlePoint;
  _Selections: TRDesignderSelections;
  _Selected: TPersistent;
  I: Integer;
begin
  if Selected=nil then Exit;
  _Selected :=  Selected;
  //如果是非可视组件的表现
  if Selected is TNonVisibleComponentDesgn then
    _Selected :=  (Selected as TNonVisibleComponentDesgn).ContainComponent
  else if ObjISNonVisibleComponent(Selected) then
  begin
    for I := 0 to FNVCHandleManager.ComponentCount - 1 do
      if (FNVCHandleManager.Components[I] as TNonVisibleComponentDesgn).ContainComponent=Selected then
      begin
        Selected :=  (FNVCHandleManager.Components[I] as TNonVisibleComponentDesgn);
        Break;
      end;
  end;

  FSelection.Add(Selected);
  if (Selected is TControl) and (Selected <> FDesignForm) then
    for D := hpdLeftUp to hpdLeft do
    begin
      FrameSize := THandlePoint.Create(FGrabHandleManager, Selected as TControl, (Selected as TControl).Parent, D);
      FrameSize.PointSize :=  FDesingerPointSize;
    end;
  if FSelection.Count>1 then
    MonitorActiveComponent(nil)
  else if (FSelection.Count=1) and (_Selected is TComponent) then
    MonitorActiveComponent(_Selected as TComponent);
  if Assigned(FSelectionChange) then
    FSelectionChange(Self);

  if FPropManager<>nil then
    try
      _Selections :=  CreateSelection;
      FPropManager.SetSelection(_Selections);
    finally
      FreeAndNil(_Selections);
    end;
end;

procedure TCustomDesigner.Align(Affect: TAlignAffect);

  procedure DoHSpace;
  var
    StartLeft, EndLeft, NewSpace: Integer;
    I: Integer;
  begin
    StartLeft :=  TControl(Selected[0]).Left;
    EndLeft   :=  TControl(Selected[SelectCount-1]).Left;
    NewSpace  :=  Round((EndLeft - StartLeft)/(SelectCount-1));
    for I := 1 to SelectCount - 2 do   //第一个最后一个不纳入其中
      TControl(Selected[I]).Left  :=  StartLeft + I * NewSpace;
  end;

  procedure DoVSpace;
  var
    StartTop, EndTop, NewSpace: Integer;
    I: Integer;
  begin
    StartTop  :=  TControl(Selected[0]).Top;
    EndTop    :=  TControl(Selected[SelectCount-1]).Top;
    NewSpace  :=  Round((EndTop - StartTop)/(SelectCount-1));
    for I := 1 to SelectCount - 2 do   //第一个最后一个不纳入其中
      TControl(Selected[I]).Top  :=  StartTop + I * NewSpace;
  end;

  procedure DoHWinCenter;
  var
    StartLeft, EndRight, NewSpace: Integer;
    HLine, HLine2: Integer; //水平中间线
    Parent: TWinControl;
    I: Integer;
  begin
    Parent    :=  GetParent;
    HLine     :=  Round(Parent.ClientWidth/2);
    StartLeft :=  TControl(Selected[0]).Left;
    EndRight  :=  TControl(Selected[SelectCount-1]).Left+TControl(Selected[SelectCount-1]).Width;
    HLine2    :=  Round((EndRight-StartLeft)/2);
    NewSpace  :=  HLine - HLine2  - StartLeft;
    for I := 0 to SelectCount - 1 do
      TControl(Selected[I]).Left  :=  TControl(Selected[I]).Left  + NewSpace;
  end;

  procedure DoVWinCenter;
  var
    StartTop, EndBottom, NewSpace: Integer;
    VLine, VLine2: Integer; //水平中间线
    Parent: TWinControl;
    I: Integer;
  begin
    Parent    :=  GetParent;
    VLine     :=  Round(Parent.ClientHeight/2);
    StartTop  :=  TControl(Selected[0]).Top;
    EndBottom :=  TControl(Selected[SelectCount-1]).Top+TControl(Selected[SelectCount-1]).Height;
    VLine2    :=  Round((EndBottom-StartTop)/2);
    NewSpace  :=  VLine - VLine2  - StartTop;
    for I := 0 to SelectCount - 1 do
      if Selected[I] is TControl then
        TControl(Selected[I]).Top :=  TControl(Selected[I]).Top  + NewSpace;
  end;

  procedure DoHSpaceInc;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      if Selected[I] is TControl then
        TControl(Selected[I]).Left  :=  TControl(Selected[I]).Left  + I*1;
  end;

  procedure DoHSpaceDec;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      if Selected[I] is TControl then
        TControl(Selected[I]).Left  :=  TControl(Selected[I]).Left  - I*1;
  end;

  procedure DoHSpaceDel;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      if TControl(Selected[I]).Left>TControl(Selected[I-1]).Left+TControl(Selected[I-1]).Width then
        TControl(Selected[I]).Left  :=  TControl(Selected[I-1]).Left+TControl(Selected[I-1]).Width;
  end;

  procedure DoVSpaceInc;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      TControl(Selected[I]).Top  :=  TControl(Selected[I]).Top  + I*1;
  end;

  procedure DoVSpaceDec;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      TControl(Selected[I]).Top  :=  TControl(Selected[I]).Top  - I*1;
  end;

  procedure DoVSpaceDel;
  var
    I: Integer;
  begin
    for I := 1 to SelectCount - 1 do
      if TControl(Selected[I]).Top>TControl(Selected[I-1]).Top+TControl(Selected[I-1]).Height then
        TControl(Selected[I]).Top  :=  TControl(Selected[I-1]).Top+TControl(Selected[I-1]).Height;
  end;

  procedure DoSendToBack;
  var
    I: integer;
  begin
    for I:= 0 to SelectCount - 1 do
      if Selected[I] is TControl then
        TControl(Selected[I]).SendToBack;
  end;

  procedure DoBringToFront;
  var
    I: integer;
  begin
    for I:= 0 to SelectCount - 1 do
      if Selected[I] is TControl then
        TControl(Selected[I]).BringToFront;
  end;

  procedure DoAlignToGrid;
  var
    I: integer;
  begin
    for I:= 0 to SelectCount - 1 do
      if Selected[I] is TControl then
        with TControl(Selected[I]) do
        begin
          Left  := Round(Left / FGridSize) * FGridSize;
          Top   := Round(Top / FGridSize) * FGridSize;
        end;
  end;

  procedure DoOtherAlign;
  var
    i: integer;
    xpos, ypos, HorBottom, VertBottom, Bottompos, rightpos: integer;
  begin
    with TControl(Selected[0]) do
    begin
      xpos        :=  Left;
      ypos        :=  Top;
      HorBottom   :=  top  +  (height div 2);
      VertBottom  :=  Left +  (width div 2);
      Bottompos   :=  Top  +  height;
      rightpos    :=  xpos +  width;
    end;

    for i:= 1 to SelectCount - 1 do
      if Selected[I] is TControl then
        with TControl(Selected[I]) do
        begin
          case Affect of
            alTop:        Top   :=  ypos;
            alLeft:       Left  :=  xpos;
            alBottom:     Top   :=  Bottompos-Height;
            alRight:      Left  :=  rightpos-Width;
            alHCenter:    Top   :=  HorBottom -  (Height div 2);
            alVCenter:    Left  :=  VertBottom - (width div 2);
          end;
        end;
  end;

  function CheckCanAlign: Boolean;
  begin
    Result  :=  SelectCount>0;
    case Affect of
      alHWinCenter,
      alVWinCenter,
      alAlignToGrid,
      alSnapToGrid,
      alSendToBack,
      alBringToFront: Result  :=  SelectCount>0;
      alHSpace,
      alVSpace: Result  :=  SelectCount>2;
      else
        Result  :=  SelectCount>1;
    end;
  end;

begin
  if not CheckCanAlign then Exit;

  ShowGrabHandle(False);
  case Affect of
    alHSpace:     DoHSpace;
    alVSpace:     DoVSpace;
    alHWinCenter: DoHWinCenter;
    alVWinCenter: DoVWinCenter;
    alHSpaceInc:  DoHSpaceInc;
    alHSpaceDec:  DoHSpaceDec;
    alHSpaceDel:  DoHSpaceDel;
    alVSpaceInc:  DoVSpaceInc;
    alVSpaceDec:  DoVSpaceDec;
    alVSpaceDel:  DoVSpaceDel;
    alAlignToGrid:DoAlignToGrid;
    alSendToBack: DoSendToBack;
    alBringToFront:DoBringToFront;
    alSnapToGrid: FSnapToGrid :=  not FSnapToGrid;
    else
      DoOtherAlign;
  end;

  ShowGrabHandle(True);
  Modified;
end;

function TCustomDesigner.CanPaste: Boolean;
begin
  Result  :=  getCanPaste;
end;

procedure TCustomDesigner.ChainCall(const MethodName, InstanceName,
  InstanceMethod: string; TypeData: PTypeData);
begin

end;

procedure TCustomDesigner.ChainCall(const MethodName, InstanceName,
  InstanceMethod: string; const AEventInfo: IEventInfo);
begin
end;

procedure TCustomDesigner.ClearSelections;
var
  I: Integer;
  Obj: TPersistent;
begin
  ShowGrabHandle(False);
  for I := FSelection.Count - 1 downto 0 do
  begin
    Obj :=  FSelection.Get(I);
    if Obj is TControl then
      ClearGrabHandle(TControl(Obj as TControl));
    Obj :=  nil;
  end;
  FSelection.ClearSelections;
end;

procedure TCustomDesigner.ClearGrabHandle(AControl: TControl);
var
  I: Integer;
begin
  for I := FGrabHandleManager.ComponentCount - 1 downto 0 do
    if (FGrabHandleManager.Components[I] is THandlePoint)
      and (THandlePoint(FGrabHandleManager.Components[I]).FControl = AControl) then
      THandlePoint(FGrabHandleManager.Components[I]).Free;
end;

procedure TCustomDesigner.ClearSelection;
begin
  ShowGrabHandle(False);
  FSelection.ClearSelections;
end;

constructor TCustomDesigner.Create(Onwer: TComponent);
begin
  inherited create(Onwer);
  FSelection            :=  TRDesignderSelections.Create;
  FPopupEditors         :=  TPopupEditors.Create(Self);
  FGrabHandleManager    :=  THandlePointManager.Create(Self);
  FNVCHandleManager     :=  TNVCHandleManager.Create(Self);
  FShowGrid             :=  true;
  FShowPopupEditor      :=  true;
  FGridSize             :=  8;
  FDesingerPointSize    :=  4;
  FMutilSelectedColor   :=  clAppWorkSpace;
  FSingleSelectedColor  :=  clBlack;
  FGridColor            :=  clGray;
  FPropManager          :=  nil;
  FDesignForm           :=  nil;
  FComponentsMonitor    :=  TList.Create;
  FDraggedList          :=  TList.Create;
end;

function TCustomDesigner.CreateComponent(ComponentClass: TComponentClass;
  Parent: TComponent; Left, Top, Width, Height: Integer): TComponent;

  procedure CheckSnapGrid;
  var
    p:  TPoint;
  begin
    if not FSnapToGrid then Exit;
  end;

var
  DesignInfo: PLongint;
  P: TPoint;
begin
  CheckSnapGrid;
  Result      :=  ComponentClass.Create(FDesignForm);
  Result.Name :=  UniqueName(ComponentClass.ClassName);
  if ObjISNonVisibleComponent(Result) then
  begin
    P :=  (Parent as TControl).ClientToScreen(Point(Left, Top));
    P :=  FDesignForm.ScreenToClient(P);
    DesignInfo  :=  @(Result.DesignInfo);
    LongRec(DesignInfo^).Lo :=  P.X;
    LongRec(DesignInfo^).Hi :=  P.Y;
    BuildDesignNonVisbleComponent(Result);
  end
  else
  begin
    TControl(Result).Parent :=  Parent as TWinControl;
    TControl(Result).Left   :=  Left;
    TControl(Result).Top    :=  Top;
    if Width<>-1 then
      TControl(Result).Width  :=  Width;
    if Height<>-1 then
      TControl(Result).Height :=  Height;
  end;
  ClearSelections;
  AddSelected(Result);
  NonCompBringFront;
  ShowGrabHandle(True);
  if Assigned(FOnAfterAddComponent) then FOnAfterAddComponent(Result);
end;

function TCustomDesigner.CreateCurrentComponent(Parent: TComponent;
  const Rect: TRect): TComponent;
begin

end;

function TCustomDesigner.CreateMethod(const Name: string;
  const AEventInfo: IEventInfo): TMethod;
begin
  Result.Code :=  nil;
  Result.Data :=  nil;
  if Assigned(FCreateMethodX) then
    FCreateMethodX(Name, AEventInfo, Result);
end;

function TCustomDesigner.CreateMethod(const Name: string;
  TypeData: PTypeData): TMethod;
begin
  Result.Code :=  nil;
  Result.Data :=  nil;
  if (Name<>'') and Assigned(FCreateMethod) then
    FCreateMethod(Name, TypeData, Result);
end;

function TCustomDesigner.CreateSelection: TRDesignderSelections;
var
  I: Integer;
  Obj: TPersistent;
begin
  Result  :=  TRDesignderSelections.Create;
  for I := 0 to FSelection.Count - 1 do
  begin
    Obj :=  FSelection.Items[I];
    if Obj is TNonVisibleComponentDesgn then
      Result.Add((Obj as TNonVisibleComponentDesgn).ContainComponent)
    else
      Result.Add(Obj);
    Obj :=  nil;
  end;
end;

destructor TCustomDesigner.Destroy;
begin
  DesignNotificationMgr.PostDesignerClosed(Self, False);
  FPropManager  :=  nil;
  if FDesignForm <> nil then
    TCrackComponent(FDesignForm).SetDesigning(False, True);
  FGrabHandleManager.Free;
  FNVCHandleManager.Free;
  FPopupEditors.Free;
  FreeAndNil(FSelection);
  FreeAndNil(FComponentsMonitor);
  FreeAndNil(FDraggedList);
  inherited Destroy;
end;

function TCustomDesigner.GetSelectionCount: Integer;
begin
  Result  :=  FSelection.Count;
end;

function TCustomDesigner.GetControls(Index: Integer): TControl;
var
  instance: TPersistent;
begin
  Result    :=  nil;
  if (Index>-1) and (Index<FSelection.Count) then
  begin
    instance  :=  FSelection.Items[Index];
    if instance is TControl then
      Result    := TControl(instance);
    instance  :=  nil;
  end;
  if result = nil then result :=  FDesignForm;
end;

function TCustomDesigner.GetCustomForm: TCustomForm;
begin
  Result := FDesignForm;
end;

function TCustomDesigner.GetDesignerExtension: string;
begin
  Result  :=  'UI';
end;

function TCustomDesigner.GetIDEOptions: TCustomIniFile;
begin
  Result  :=  nil;
end;

function TCustomDesigner.GetIsControl: Boolean;
begin
  Result := TCrackControl(FDesignForm).IsControl;
end;

function TCustomDesigner.GetIsDormant: Boolean;
begin

end;

function TCustomDesigner.GetMethodName(const Method: TMethod): string;
begin
  Result  :=  '';
  if Assigned(FGetMethodName) then
    FGetMethodName(Method, Result);
end;

procedure TCustomDesigner.GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
begin
  if Assigned(FOnGetMethods) then
    FOnGetMethods(TypeData, Proc);
end;

procedure TCustomDesigner.GetMethods(const AEventInfo: IEventInfo;
  Proc: TGetStrProc);
begin
  if Assigned(FOnGetMethodsX) then
    FOnGetMethodsX(AEventInfo, Proc);
end;

function TCustomDesigner.GetObject(const Name: string): TPersistent;
begin
  Result  :=  FindComponent(Name);
end;

function TCustomDesigner.GetObjectName(Instance: TPersistent): string;
begin
  Result  :=  '';
  if (Instance<>nil)  then
    Result  :=  Instance.GetNamePath;
end;

procedure TCustomDesigner.GetObjectNames(TypeData: PTypeData;
  Proc: TGetStrProc);
var
  I: Integer;
begin
  if FDesignForm<>nil then
    for I := 0 to FDesignForm.ComponentCount - 1 do
      if GetTypeData(FDesignForm.Components[I].ClassInfo)=TypeData then
        Proc(FDesignForm.Components[I].Name);
end;

function TCustomDesigner.GetParent: TWinControl;
var
  cur: TControl;
begin
  if SelectCount = 0 then
    Result := FDesignForm
  else
  begin
    cur :=  TControl(Selected[0]);
    if cur is TWinControl then
      result  :=  cur as TWinControl
    else
      result :=  cur.Parent;
    while (Result <> nil) and (not (csAcceptsControls in Result.ControlStyle)) do
      Result := Result.Parent;
  end;
  if Result=nil {is TNonVisibleComponentDesgn} then
    Result  :=  FDesignForm;
end;

function TCustomDesigner.GetPathAndBaseExeName: string;
begin
  Result  :=  Application.ExeName;
end;

function TCustomDesigner.GetPrivateDirectory: string;
begin
  Result  :=  Application.ExeName;
end;

procedure TCustomDesigner.GetProjectModules(Proc: TGetModuleProc);
begin

end;

function TCustomDesigner.GetRoot: TComponent;
begin
  Result := FDesignForm;
end;

function TCustomDesigner.GetRootClassName: string;
begin
  Result  :=  '';
  if FDesignForm<>nil then
    Result  :=  FDesignForm.ClassName;
end;

function TCustomDesigner.GetScrollRanges(const ScrollPosition: TPoint): TPoint;
begin

end;

function TCustomDesigner.GetSelectCount: Integer;
begin
  Result  :=  FSelection.Count;
end;

function TCustomDesigner.GetSelected(Index: Integer): TPersistent;
begin
  Result  :=  FSelection.Items[Index];
end;

procedure TCustomDesigner.GetSelections(const List: IDesignerSelections);
var
  I: Integer;
begin
  for I :=  0 to FSelection.Count -1 do
    List.Add(FSelection.Items[I]);
end;

function TCustomDesigner.GetShiftState: TShiftState;
begin
end;

function TCustomDesigner.ObjISNonVisibleComponent(Obj: TPersistent): boolean;
begin
  Result  :=  Obj.InheritsFrom(TComponent) and not Obj.InheritsFrom(TControl);
end;

function TCustomDesigner.OnMessage(Sender: TControl;
  var Message: TMessage): Boolean;
  {**GetPos
  *对于当前选中的控件如果是组合型的则必须要将鼠标的位置换算
  *到其真正的控件上，否则无法实现拖放等操作
  **}
  function GetPos(c: TControl; msg: TWMMouse): TPoint;
  var
    Origin: TPoint;
    p: TPoint;
  begin
    p :=  Mouse.CursorPos;
    result  :=  c.ScreenToClient(p);
  end;

var
  Shift: TShiftState;
  Button: TMouseButton;
begin
  {**
  *在这里进行判断控件是否为组合型，如果是则将Sender替换
  *其他地方的Sender就是这里换算出来的
  **}
  if (Sender <> FDesignForm) and {防止对无父窗体对象换算}(Sender.Parent <> nil)
      and not (Sender is TNonVisibleComponentDesgn) and not IsDesignControl(Sender) then
    Sender := getControlByEliminateAssembled(Sender);
  Result := ((Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST))
    or ((Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST));
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK: Button := mbLeft;
    WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK: Button := mbRight;
    WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK: Button := mbMiddle;
  end;
  Shift :=  KeysToShiftState(TWMMouse(Message).Keys);
  if (Message.Msg = WM_LBUTTONDBLCLK) or (Message.Msg = WM_RBUTTONDBLCLK) or
        (Message.Msg = WM_MBUTTONDBLCLK) then
    Include(Shift, ssDouble);

  case Message.Msg of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN,
    WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
      MouseDown(Sender, Button, Shift,
        GetPos(Sender, TWMMouse(Message)).X,
        GetPos(Sender, TWMMouse(Message)).y);
    WM_MOUSEMOVE:
      begin
        MouseMove(Sender, Shift,
          GetPos(Sender, TWMMouse(Message)).X,
          GetPos(Sender, TWMMouse(Message)).y);
      end;
    WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
      begin
        MouseUp(
          Sender,
          Button,
          Shift,
          GetPos(Sender, TWMMouse(Message)).X,
          GetPos(Sender, TWMMouse(Message)).y);
      end;
    WM_KEYDOWN:
      begin
        KeyDown(
          Sender,
          TWMKey(Message).CharCode,
          KeyDataToShiftState(TWMKey(Message).KeyData)
          );
      end;
    WM_KEYUP:
    begin
      if Assigned(FOnKeyUp) then
        FOnKeyUp( Sender,
          TWMKey(Message).CharCode,
          KeyDataToShiftState(TWMKey(Message).KeyData));
    end;
    WM_Char:
    begin
      DoKeyPress(Sender, TWMKey(Message));
    end;

  end;
  if Sender = FDesignForm then
    Result := False;
end;

function TCustomDesigner.IsComponentHidden(Component: TComponent): Boolean;
begin
  Result  :=  PackageManager.ISNoIcon(Component.ClassType);
end;

function TCustomDesigner.IsComponentLinkable(Component: TComponent): Boolean;
begin

end;

function TCustomDesigner.IsDesignControl(Control: TControl): Boolean;
begin
  Result  :=  Control is TNonVisibleComponentName;
end;

function TCustomDesigner.IsDesignMsg(Sender: TControl;
  var Message: TMessage): Boolean;

  procedure RebuildGrabHandle;
  begin
    if FDragging then Exit;
    //修改八点位置
    if (Sender<>FDesignForm) and ControlInSelection(Sender) then
    begin
      ShowGrabHandle(False);
      ShowGrabHandle(True);
    end
  end;

begin
  Result  :=  False;

  if Message.Msg = CM_FOCUSCHANGED then
  begin
    Result := True;
    Exit;
  end;

  if (Message.Msg >= WM_NCMOUSEMOVE) and (Message.Msg <= WM_NCXBUTTONDBLCLK) and (Sender = FDesignForm) then
  begin
    case Message.Msg of
      WM_NCLBUTTONDOWN:
        SelectComponent(GetRoot);
      WM_NCRBUTTONDOWN:
      begin
        SelectComponent(GetRoot);
        ShowComponentEditor;
      end;
    end;
    Result := True;
    Exit;
  end;

  if (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MBUTTONDBLCLK) then
  begin
    if IsDesignControl(Sender) then Exit;

    Result := ((Sender.Perform(CM_DESIGNHITTEST, Message.WParam, Message.LParam) = 0) or
                (Message.Msg >= WM_RBUTTONDOWN)) and
               ((Message.Msg >= WM_RBUTTONDOWN) and (Message.Msg <= WM_RBUTTONDBLCLK) or
               (GetCursor = Screen.Cursors[crDefault]) or Dragging);
    if not Result then
    begin
      case Message.Msg of
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: SelectComponent(Sender);
      end;
      Exit;
    end;
  end;

  if (Sender is THandlePoint) then Exit;
  //判断是否处于设计时期接收消息
  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST,
    WM_KEYFIRST..WM_KEYLAST:
      Result := OnMessage(Sender, Message);
    WM_SYSCOMMAND:
      case Message.WParam of
        SC_SIZE, 61446, 61442, 61448:
          Result  :=  False;
        else
          Result  :=  True;
      end;
    WM_SIZE:
      RebuildGrabHandle;
    WM_WINDOWPOSCHANGED:
    begin
      if (SelectCount=1) and not FDragging then
        if Sender is TWinControl then
          PostMessage((Sender as TWinControl).Handle, Form_POSCHANGED, 0, 0)
        else
          Sender.Perform(Form_POSCHANGED, 0, 0);
    end;
    Form_POSCHANGED:
    begin
      if Assigned(FOnFormSizeChange) then
        FOnFormSizeChange(FDesignForm);
      RebuildGrabHandle;
    end;
    WM_NCACTIVATE:
      if Sender=FDesignForm then
        TWMNCACTIVATE(message).Active :=  True;
    WM_PAINT: Result := PaintControl(Sender, Message);
  end;
end;

function TCustomDesigner.IsSourceReadOnly: Boolean;
begin
  Result  :=  False;
end;

procedure TCustomDesigner.KeyDown(Sender: TControl; var Key: Word;
  Shift: TShiftState);

  procedure Next_DesignFocus;
  begin
  end;

var
  I: Integer;
begin
  if (SelectCount = 0) or (Key in [VK_CONTROL, VK_SHIFT]) then Exit;
  //处理复制\粘贴\剪切
  if ssCtrl in Shift then
  begin
    case Key of
      67: Copy; //Ctrl + C
      88: Cut; //Ctrl + x
      86: Paste; //Ctrl + V
    end;
  end;
  //
  if (SelectCount = 1) and (Selected[0]=FDesignForm) then Exit;

  if ssCtrl in Shift then
  begin
    case Key of
      VK_UP:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                TControl(Selected[I]).Top :=  TControl(Selected[I]).Top-1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_DOWN:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                TControl(Selected[I]).Top :=  TControl(Selected[I]).Top+1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_LEFT:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                TControl(Selected[I]).Left :=  TControl(Selected[I]).Left-1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_RIGHT:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                TControl(Selected[I]).Left :=  TControl(Selected[I]).Left+1;
          finally
            ShowGrabHandle(True);
          end;
        end;
    end;
  end else
  if ssShift in Shift then    //按住SHIFT调整大小
  begin
    case Key of
      VK_UP:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                if TControl(Selected[I]).Height - 1 > 1 then
                  TControl(Selected[I]).Height := TControl(Selected[I]).Height - 1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_DOWN:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                if TControl(Selected[I]).Height - 1 > 1 then
                  TControl(Selected[I]).Height := TControl(Selected[I]).Height + 1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_LEFT:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                if TControl(Selected[I]).Width - 1 > 1 then
                  TControl(Selected[I]).Width := TControl(Selected[I]).Width - 1;
          finally
            ShowGrabHandle(True);
          end;
        end;
      VK_RIGHT:
        begin
          ShowGrabHandle(False);
          try
            for I := 0 to SelectCount - 1 do
              if Selected[I] is TControl then
                if TControl(Selected[I]).Width - 1 > 1 then
                  TControl(Selected[I]).Width := TControl(Selected[I]).Width + 1;
          finally
            ShowGrabHandle(True);
          end;
        end;
    end;
  end else
  begin
    case Key of
      VK_UP,
      VK_DOWN,
      VK_LEFT,
      VK_RIGHT:   Next_DesignFocus;
      VK_DELETE:  Delete; //删除组件
    end;
  end;
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TCustomDesigner.RemoveFromForm(c: TComponent);
var
  newC1: TComponent;
begin
  newC1 :=  C;
  if TControl(newC1) is TNonVisibleComponentDesgn then
  begin
    newC1 :=  TNonVisibleComponentDesgn(newC1).ContainComponent;
    ClearGrabHandle(c as TNonVisibleComponentDesgn);
    c.Free;
  end;
  newC1.Free;
//  if c is TControl then
//  begin
//    newC1  :=  c;
//    if TControl(newC1) is TNonVisibleComponentDesgn then
//    begin
//      newC2  :=  TNonVisibleComponentDesgn(newC1).ContainComponent;
//      if Assigned(FOnBeforeDelete) then
//        FOnBeforeDelete(Self, newC2);
//      MonitorDeleteComponent(newC2);
//      FDesignForm.RemoveComponent(newC2);
//      newC2.Free;
//      newC2 :=  nil;
//    end
//    else if Assigned(FOnBeforeDelete) then
//      FOnBeforeDelete(Self, newC1);
//    Remove(TControl(newC1));
//    newC1.Free;
//    newC1  :=  nil;
//  end;
end;

procedure TCustomDesigner.MakeComponentLinkable(Component: TComponent);
begin

end;

function TCustomDesigner.MethodExists(const Name: string): Boolean;
begin
  Result  :=  False;
  if Assigned(FOnFindMethod) then
    FOnFindMethod(Name, Result);
end;

function TCustomDesigner.MethodFromAncestor(const Method: TMethod): Boolean;
begin
  Result  :=  False;
  if Assigned(FMethodFromAncestor) then
    FMethodFromAncestor(Method, Result);
end;

procedure TCustomDesigner.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin
  FPropManager.ModalEdit(EditKey, ReturnWindow);
end;

procedure TCustomDesigner.RenameMethod(const CurName, NewName: string);
begin
  if Assigned(FOnReNameMethod) then
    FOnReNameMethod(CurName, NewName);
end;

procedure TCustomDesigner.Resurrect;
begin

end;

procedure TCustomDesigner.Revert(Instance: TPersistent; PropInfo: PPropInfo);
begin
  if (Instance is TComponent) and (PropInfo^.PropType^.Name='TComponentName') then
  begin
    FNVCHandleManager.UpdateComponentName(Instance as TComponent);
    if Assigned(FOnComponentNameChanged) then
      FOnComponentNameChanged(Instance as TComponent);
  end;
end;

procedure TCustomDesigner.Modified;
begin
  DoModified;
end;

procedure TCustomDesigner.ModuleFileNames(var ImplFileName, IntfFileName,
  FormFileName: string);
begin

end;

procedure TCustomDesigner.MonitorActiveComponent(Component: TComponent);
var
  I: Integer;
  ic: IComponentsMonitor;
begin
  for I := 0 to FComponentsMonitor.Count - 1 do
    PComponentsMonitor(FComponentsMonitor.Items[I])^.ComponentsMonitor.ActiveComponent(Component);
end;

procedure TCustomDesigner.MonitorAddCompoent(Component: TComponent);
var
  I: Integer;
begin
  for I := 0 to FComponentsMonitor.Count - 1 do
    PComponentsMonitor(FComponentsMonitor.Items[I])^.ComponentsMonitor.AddComponent(Component);
end;

procedure TCustomDesigner.MonitorDeleteComponent(Component: TComponent);
var
  I: Integer;
begin
  for I := 0 to FComponentsMonitor.Count - 1 do
    PComponentsMonitor(FComponentsMonitor.Items[I])^.ComponentsMonitor.DeleteComponent(Component);
end;

procedure TCustomDesigner.MouseDown(Sender: TControl; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure mbRightBeforeSelect;
    var
      Sels: array of TControl;
      i, j: integer;
      Parent: TWinControl;
    begin
      SetLength(Sels, SelectCount);
      for i :=  0 to SelectCount - 1 do
        if Selected[I] is TControl then
          Sels[i] :=  TControl(Selected[I]);
      ClearSelections;

      Parent  :=  Sender.Parent;
      for i :=  0 to  Parent.ControlCount - 1 do
        for j :=  0 to High(Sels) do
          if Parent.Controls[i] = Sels[j] then
          begin
            AddSelected(Parent.Controls[i]);
            break;
          end;
      Parent  :=  nil;
      SetLength(Sels, 0);
    end;
var
  CtrlIndex: Integer;
  i: integer;
begin
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    EditComponent(GetComponentFromSelect(Sender));
    Exit;
  end;

  ShowGrabHandle(False);  //隐藏选择
  if not (Dragging and (Button = mbLeft)) and not IsDesignControl(Sender) then
  begin
    CtrlIndex := FSelection.IndexOf(Sender);
    if (ssShift in Shift) then //按Shift多选
    begin
      if Sender = FDesignForm then  Exit;//多选不能添加Root或者Form
      if CtrlIndex = -1 then
      begin
        AddSelected(Sender);
        Dragging := False;
      end else
      begin
        Remove(Sender);

      end;
    end else
    if (Button = mbLeft) and ((ssCtrl in Shift) or (Sender = FDesignForm)) then //按住Shift或者点击的是Root就框选
    begin
      ClearSelections();
      if (Sender is TWinControl) then
      begin
        AddSelected(Sender);
        if (TWinControl(Sender).ControlCount > 0) then
        begin
          FPointStart := Sender.ClientToScreen(Point(X, Y));  //记录刚才点下的时候的起始坐标
          FOldRect    := Rect(X, Y, X + 1, Y + 1);  //记录点下位置的Rect
          FSelecting  := True;
          SetCaptureControl(Sender);
        end;
      end;
    end else   //没按Shift也没按Ctrl点击.那就添加自己到选择的控件组中 .Root和Form不能和别的控件同时在组中
    if (Button = mbLeft) or     //左键单击
        ((Button = mbRight) and (    //右键选择
          (SelectCount <= 1)    //当前设计控件个数小于2等同于左键
          or (Sender = FDesignForm))   //当前选择控件是Root等同于左键
          or (not ControlInSelection(Sender))  //当前有多个控件正在设计，但是选择的控件没有在其中，等同于左键
    ) then
    begin
      if (CtrlIndex = -1) or (Button = mbRight) then
      begin
        ClearSelections();
        AddSelected(Sender);
      end;
      Dragging := Button = mbLeft;
      if Dragging then begin
        FDraggingControl := Sender;
        MouseLock(Sender);
        FBeforDragPos := Sender.ClientToScreen(Point(X, Y));
      end;
    end else
    if ((Button = mbRight) and (SelectCount > 1)) then
      mbRightBeforeSelect;//多选后右键清除部分选择
  end;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button,  Shift, X, Y);
  if Assigned(FOnElementSelected) then
    FOnElementSelected(Self);
end;

procedure TCustomDesigner.MouseFree;
begin
  SetCaptureControl(nil);
  ClipCursor(@FMouseRect);
end;

procedure TCustomDesigner.MouseLock(Sender: TControl);
var
  R: TRect;
begin
  SetCaptureControl(Sender);
  GetClipCursor(FMouseRect);

  if Sender.Parent = nil then
    Exit;

  R := Sender.Parent.ClientRect;
  R.TopLeft := Sender.Parent.ClientToScreen(R.TopLeft);
  R.BottomRight := Sender.Parent.ClientToScreen(R.BottomRight);
  ClipCursor(@R); //把鼠标锁定在固定区域
end;

procedure TCustomDesigner.MouseMove(Sender: TControl; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  CPos: TPoint;
  DC: HDC;
begin
  if Dragging then
  begin
    if FLocked then exit;
    CPos := Mouse.CursorPos;
    for I := FSelection.Count - 1 downto 0 do
      if Selected[I] is TControl then
      begin
        if TControl(Selected[I]).Parent = Sender.Parent then //如果都是同一个Paren的话
        begin
          TControl(Selected[I]).Left  := TControl(Selected[I]).Left - (FBeforDragPos.X - CPos.X);
          TControl(Selected[I]).Top   := TControl(Selected[I]).Top - (FBeforDragPos.Y - CPos.Y);
        end;
      end
      else
        Remove(I);
    FBeforDragPos := CPos;
  end
  else if FSelecting then
  begin
    FPointEnd := Sender.ClientToScreen(Point(X, Y));
    FNewRect := PointToRect(FPointStart, FPointEnd);
    DC := GetDC(0);
    DrawFocusRect(DC, FOldRect);
    DrawFocusRect(DC, FNewRect);
    ReleaseDC(0, DC);
    FOldRect := FNewRect;
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);
end;

procedure TCustomDesigner.MouseUp(Sender: TControl; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DC: HDC;
begin
  if Dragging then
  begin
    MouseFree();
    Modified;
    Dragging := False;
  end;
  if FSelecting then
  begin
    DC := GetDC(0);
    DrawFocusRect(DC, FOldRect);
    ReleaseDC(0, DC);
    FSelecting := False;
    SetCaptureControl(nil);
    if Sender is TWinControl then
    begin
      FOldRect.TopLeft := Sender.ScreenToClient(FOldRect.TopLeft);
      FOldRect.BottomRight := Sender.ScreenToClient(FOldRect.BottomRight);
      FOldRect := PointToRect(FOldRect.TopLeft, FOldRect.BottomRight);
      AddRectControls(TWinControl(Sender), FOldRect);
    end;
  end;
  ShowGrabHandle(True); //重新显示选择项目
  if FShowPopupEditor and (Button = mbRight) then //显示右键菜单
      ShowComponentEditor;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button,  Shift, X, Y);
end;

procedure TCustomDesigner.CanInsertComponent(AComponent: TComponent);
begin

end;

procedure TCustomDesigner.DesignerNotification(AnObject: TPersistent; Operation: TOperation);
var
  Index: Integer;
begin
  case Operation of
    opRemove:
      begin
        if AnObject is TComponent then
          MonitorDeleteComponent(AnObject as TComponent);
        if AnObject is TControl then
          ClearGrabHandle(AnObject as TControl);
        if FSelection<>nil then
        begin
          Index := FSelection.IndexOf(AnObject);
          if Index <> -1 then
            Remove(Index);
        end;
        if AnObject = FDesignForm then
        begin
          TCrackComponent(FDesignForm).SetDesigning(False, True);
          FDesignForm := nil;
        end;
      end;
    opInsert:
      begin
        if not (AnObject is TComponent) then Exit;
        MonitorAddCompoent(AnObject as TComponent);
      end;
  end;
end;

function TCustomDesigner.PaintControl(Sender: TControl;
  var Message: TMessage): Boolean;
var
  IsSel, IsSelMark, IsGrid: Boolean;
  DC: HDC;
  PS: PAINTSTRUCT;
  br: HBrush;

  procedure DrawMRect(DC: HDC; Rect: TRect);
  begin
     if RectVisible(DC, Rect) then
      begin
       FillRect(DC, Rect, br);
       with Rect do ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
  end;

begin
  DC        := Message.WParam;
  IsSel     := FSelection.IndexOf(Sender) <> -1;
  IsSelMark := (SelectCount > 1) and IsSel;
  IsGrid    := FShowGrid and (Sender=FDesignForm) and
              not (Root is TDataModule) and
              (Sender is TWinControl) and
              (csAcceptsControls in (Sender as TWinControl).ControlStyle);

  Result := False;
  if not (IsSelMark or IsGrid) then Exit;

  if (DC = 0) and (Sender is TWinControl) then
   begin
    DC := BeginPaint((Sender as TWinControl).Handle, PS);
    if IsGrid then
      PaintDesignGrid(DC, Sender as TWinControl);
    SelectClipRgn(DC, 0);
    Message.WParam := DC;
    TWinCtrolCrack(Sender).PaintHandler(TWMPaint(Message));
    Message.WParam := 0;
    SelectClipRgn(DC, 0);
   end;

  if (Message.WParam = 0) and (Sender is TWinControl) then
    begin
     EndPaint((Sender as TWinControl).Handle, PS);
     Result := True;
    end;
end;

procedure TCustomDesigner.PaintDesignGrid(DC: HDC; Ctrl: TControl);
var
  r: TRect;
  x, y: integer;
  b: Boolean;
  oldDC: integer;
  offs: TPoint;
begin
  if Ctrl = nil then Exit;

  oldDc := SaveDC(DC);
  try
    GetClipBox(DC, r);

    if Ctrl is TScrollingWinControl then
     with Ctrl as TScrollingWinControl do
      begin
       GetWindowOrgEx(DC, offs);
       x := HorzScrollBar.Position;
       y := VertScrollBar.Position ;
       SetWindowOrgEx(DC, offs.X + x, offs.Y + y, nil);
       OffsetRect(r, x, y);
      end else
    if Ctrl is TWinControl then
     with TWinCtrolCrack(Ctrl as TWinControl) do
      if BevelWidth > 0 then
       begin
        if r.Left < BevelWidth then r.Left := BevelWidth;
        if r.Top < BevelWidth then r.Top := BevelWidth;
        if r.Right > ClientWidth - BevelWidth then r.Right := ClientWidth - BevelWidth;
        if r.Bottom > ClientHeight - BevelWidth then r.Bottom := ClientHeight - BevelWidth;
       end;

    b := r.Left mod FGridSize > 0;
    r.Left := (r.Left div FGridSize) * FGridSize;
    if b then Inc(r.Left, FGridSize);
    b := r.Top mod FGridSize > 0;
    r.Top := (r.Top div FGridSize) * FGridSize;
    if b then Inc(r.Top, FGridSize);

    x := r.Left;
    while x <= r.Right do
     begin
      y := r.Top;
      while y <= r.Bottom do
       begin
        Windows.SetPixel(DC, X, Y, clGray);
        inc(y, FGridSize);
       end;
      inc(x, FGridSize);
     end;
  finally
    RestoreDC(DC, oldDC);
  end;
end;

procedure TCustomDesigner.PaintGrid;
begin
  if (FDesignForm=nil) or not FShowGrid then  Exit;
  PaintDesignGrid(FDesignForm.Canvas.Handle, FDesignForm);
end;

procedure TCustomDesigner.PaintMenu;
begin
end;

function TCustomDesigner.OwnerCheck(Sender: TControl; CheckOnwer: TComponent): Boolean;
var
  W: TComponent;
begin
  Result := False;
  W := Sender.Owner;
  while W <> nil do
  begin
    if W = CheckOnwer then
    begin
      Result := True;
      Exit;
    end;
    W := W.Owner;
  end;
end;

procedure TCustomDesigner.Remove(Index: Integer);
var
  ins:  TPersistent;
begin
  if (Index>-1) and (Index<FSelection.Count) then
  begin
    ins :=  FSelection.Items[Index];
//    if ins is TComponent then
//      MonitorDeleteComponent(ins as TComponent);
//    if ins is TControl then
//      ClearGrabHandle(TControl(ins));
    FSelection.Delete(Index);
    if FPropManager<>nil then
      FPropManager.SetSelection(FSelection);
    ins :=  nil;
  end;
end;

procedure TCustomDesigner.Remove(AControl: TControl);
begin
  MonitorDeleteComponent(AControl);
  FSelection.Delete(AControl);
  if FPropManager<>nil then
    FPropManager.SetSelection(FSelection);
  ClearGrabHandle(AControl);
end;

procedure TCustomDesigner.Select(Items: array of TPersistent);
var
  I: Integer;
begin
  ClearSelections;
  for I := Low(Items) to High(Items) do
    AddSelected(Items[I]);
  ShowGrabHandle(True);
end;

procedure TCustomDesigner.SelectAll;
var
  I: Integer;
begin
  ShowGrabHandle(False);
  ClearSelections;
  for I := 0 to GetRoot.ComponentCount - 1 do
    AddSelected(GetRoot.Components[I]);
  ShowGrabHandle(True);
end;

procedure TCustomDesigner.SelectComponent(Instance: TPersistent);
begin
  Select([Instance]);
end;

procedure TCustomDesigner.SelectComponent(const ADesignObject: IDesignObject);
var
  instance: IDesignPersistent;
begin
  if Supports(ADesignObject,IDesignPersistent,instance) then
    SelectComponent(instance.Persistent);
  instance  :=  nil;
end;

procedure TCustomDesigner.SelectItemName(const PropertyName: string);
begin

end;

procedure TCustomDesigner.SetCustomForm(Value: TCustomForm);
begin
  FDesignForm := Value;
  if Value <> nil then
  begin
    Value.Designer := Self;
    Select([FDesignForm]);
    FDesignForm.FreeNotification(Self);
  end;
end;

procedure TCustomDesigner.SetDragging(const Value: Boolean);
begin
  FDragging := Value;
  ShowGrabHandle(not Value);
end;

procedure TCustomDesigner.SetIsControl(Value: Boolean);
begin
  if FDesignForm is TControl then
    TCrackControl(FDesignForm).IsControl := Value;
end;

procedure TCustomDesigner.SetPropManager(const Value: TuPropertiesManager);
begin
  FPropManager := Value;
  FPropManager.Designer :=  Self;
end;

procedure TCustomDesigner.SetSelections(const List: IDesignerSelections);
var
  I: Integer;
begin
  ClearSelections;
  if List<>nil then
    for I := 0 to List.Count - 1 do
      AddSelected(List.Items[I]);
  ShowGrabHandle(True);
end;

procedure TCustomDesigner.ShowGrabHandle(const Show: boolean);
var
  I: Integer;
  ISMulSel: boolean;
begin
  ISMulSel  :=  SelectCount > 1;
  for I := 0 to FGrabHandleManager.ComponentCount - 1 do
    if (FGrabHandleManager.Components[I] is THandlePoint) then
    begin
      if ISMulSel then
        THandlePoint(FGrabHandleManager.Components[I]).Color := FMutilSelectedColor
      else
        THandlePoint(FGrabHandleManager.Components[I]).Color := FSingleSelectedColor;


      THandlePoint(FGrabHandleManager.Components[I]).Visible := Show and
        ((SelectCount = 1) or
        ((SelectCount > 1) and (THandlePoint(FGrabHandleManager.Components[I]).FDirect in [hpdLeftUp, hpdLeftDown, hpdRightUp, hpdRightDown])));
      THandlePoint(FGrabHandleManager.Components[I]).Pos();
    end;
end;

procedure TCustomDesigner.ShowMethod(const Name: string);
begin
  if Assigned(FOnShowMethod) then
    FOnShowMethod(Name);
end;

procedure TCustomDesigner.Size(Affect: TSizeAffect; Value: Integer);

  procedure SameWidth;
var
  I: Integer;
  newW: Integer;
begin
  newW  :=  -1;
  if (SelectCount>0) then
    for I := 0 to SelectCount - 1 do
      if Selected[I] is TControl then
      begin
        if newW=-1 then
          newW  :=  (Selected[I] as TControl).Width
        else
          (Selected[I] as TControl).Width  :=  newW;
      end;
  end;

  procedure SameHeight;
  var
    I: Integer;
    newH: Integer;
  begin
    newH  :=  -1;
    if (SelectCount>0) then
      for I := 0 to SelectCount - 1 do
        if Selected[I] is TControl then
        begin
          if newH=-1 then
            newH  :=  (Selected[I] as TControl).Height
          else
            (Selected[I] as TControl).Height  :=  newH;
        end;
  end;

  procedure SameWidthHeight;
  var
    I: Integer;
    newH,
    newW: Integer;
  begin
    newH  :=  -1;
    newW  :=  -1;
    if (SelectCount>0) then
      for I := 0 to SelectCount - 1 do
        if Selected[I] is TControl then
        begin
          if newH=-1 then
          begin
            newH  :=  (Selected[I] as TControl).Height;
            newW  :=  (Selected[I] as TControl).Width;
          end
          else
            (Selected[I] as TControl).Height  :=  newH;
            (Selected[I] as TControl).Width   :=  newW;
        end;
  end;

begin
  case Affect of
    saNothing: ;
    saHGrow: ;
    saHShrink: ;
    saHAbsolute: ;
    saVGrow: ;
    saVShrink: ;
    saVAbsolute: ;
    saWidths:   SameWidth;
    saHeights:  SameHeight;
    saWidthHeight: SameWidthHeight;
    saSizeToGrid: ;
  end;
end;

function TCustomDesigner.UniqueName(const BaseName: string): string;
var
  i: Integer;
  UName: string;
begin
  UName :=  BaseName;
  if (Length(UName) >= 2) and (UName[1] in ['t', 'T']) then
    UName := System.Copy(UName, 2, MaxInt);
  i := 0;
  repeat
    Inc(i);
    Result := UName + IntToStr(i);
  until FDesignForm.FindComponent(Result) = nil;
end;

procedure TCustomDesigner.UnRegisterComponentsMonitor(
  Monitor: IComponentsMonitor);
var
  I: Integer;
  MR: PComponentsMonitor;
begin
  for I := 0 to FComponentsMonitor.Count - 1 do
    if PComponentsMonitor(FComponentsMonitor.Items[I])^.ComponentsMonitor=Monitor then
    begin
      MR  :=  PComponentsMonitor(FComponentsMonitor.Items[I]);
      FComponentsMonitor.Delete(I);
      Dispose(MR);
      Break;
    end;
end;

procedure TCustomDesigner.UpdateDrag;
begin

end;

procedure TCustomDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  if (NewName<>'') and not IsValidIdent(NewName) or (FDesignForm.FindComponent(NewName)<>nil) then
    raise Exception.CreateResFmt(@SDuplicateName, [NewName]);
end;

procedure TCustomDesigner.DoKeyPress(Sender: TControl; var Message: TWMKey);
var
  Ch: Char;
begin
  Ch := Char(TWMKey(Message).CharCode);
  ModalEdit(Ch, nil);
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Sender, Ch);
  inherited;
end;

procedure TCustomDesigner.DoModified;
begin
  FModified  :=  True;
  if FPropManager<>nil then
    FPropManager.RefreshPainter;
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TCustomDesigner.Design(OwnerForm: TCustomForm);
begin
  DesignForm := OwnerForm;
  BuildAllDesignNonVisbleComponent;
  TCrackComponent(FDesignForm).SetDesigning(True, True);
  OwnerForm.Refresh;
  if Assigned(FOnBeginDesign) then
    FOnBeginDesign(Self);
end;

function TCustomDesigner.Designer60GetRoot: TComponent;
begin
  Result  :=  FDesignForm;
end;

procedure TCustomDesigner.Designer60Modified;
begin
  DesignNotificationMgr.PostItemsModified(Self);
  DoModified;
end;

function TCustomDesigner.DesignerUniqueName(const BaseName: string): string;
begin
  Result  :=  UniqueName(BaseName);
end;

function TCustomDesigner.QuitDesign: boolean;
var
  i: integer;
begin
  if FDesignForm  = nil then exit;
  DesignNotificationMgr.PostDesignerClosed(Self, False);
  ShowGrabHandle(False);
  TCrackComponent(FDesignForm).SetDesigning(False, True);
  for i :=  FNVCHandleManager.ComponentCount - 1 downto 0 do
  begin
    Remove(TControl(FNVCHandleManager.Components[I]));
    FNVCHandleManager.Components[I].Free;
  end;
  FDesignForm.Designer := nil;
  FDesignForm.Refresh;
  FDesignForm :=  nil;
  if Assigned(FOnQuitDesign) then
    FOnQuitDesign(Self);
end;

procedure TCustomDesigner.ReaderSetName(Reader: TReader; Component: TComponent;
  var Name: string);
begin
  if (Reader.Root = FDesignForm) and (FDesignForm.FindComponent(Name) <> nil) then
    Name := UniqueName(Component.ClassName);
end;

procedure TCustomDesigner.RegisterComponentEditor(
  AComponentClass: TComponentClass; AEditorClass: TRDComponentEditorClass);
begin
  RegisterComponentEditor(AComponentClass, AEditorClass);
end;

procedure TCustomDesigner.RegisterComponentsMonitor(
  Monitor: IComponentsMonitor);
var
  mr: PComponentsMonitor;
begin
  new(mr);
  mr^.ComponentsMonitor :=  Monitor;
  FComponentsMonitor.Add(mr);
end;

procedure TCustomDesigner.RegisterPropertyEditor(PropertyType: PTypeInfo;
  ComponentClass: TClass; const PropertyName: string;
  EditorClass: TRDPropertyEditorClass);
begin
  RegisterPropertyEditor(PropertyType, ComponentClass, PropertyName, EditorClass);
end;

procedure TCustomDesigner.BuildDesignNonVisbleComponent(AComponent: TComponent);
var
  ncd: TNonVisibleComponentDesgn;
  DesignInfo: Longint;        //定义为^Longint将会导致读取位置失败
  AIco: TBitmap;
  CanShow: boolean;
begin
  if PackageManager.ISNoIcon(AComponent.ClassType) then Exit;
  CanShow     :=  True;
  if Assigned(FOnNonVisbleComponentCreate) then
    FOnNonVisbleComponentCreate(AComponent, CanShow);
  if CanShow then
  begin
    DesignInfo  :=  AComponent.DesignInfo;
    ncd         :=  TNonVisibleComponentDesgn.Create(FNVCHandleManager, AComponent, FDesignForm);
    ncd.Left    :=  LongRec(DesignInfo).Lo;
    ncd.Top     :=  LongRec(DesignInfo).Hi;
    AIco        :=  PackageManager.GetComponentICON(AComponent);
    ncd.ICon    :=  AIco;
    TCrackComponent(ncd).SetDesigning(True, True);
  end;
end;

function TCustomDesigner.GetSelection(Index: Integer): TPersistent;
begin
  Result  :=  nil;
  if (Index>-1) and (Index<FSelection.Count) then
  begin
    Result  :=  FSelection.Items[Index];
    if Result is TNonVisibleComponentDesgn then
      Result  :=  TNonVisibleComponentDesgn(Result).ContainComponent;
    if Result = nil then Result :=  FDesignForm;
  end;
end;

procedure TCustomDesigner.BuildAllDesignNonVisbleComponent;
  function IsHasNVC(c: TComponent): boolean;
  var
    i: integer;
  begin
    result  :=  false;
    for i :=  0 to FNVCHandleManager.ComponentCount - 1 do
      if TNonVisibleComponentDesgn(FNVCHandleManager.Components[i]).ContainComponent = c then
      begin
        result  :=  True;
        break;
      end;
  end;
var
  i: integer;
begin
  for i :=  0 to FDesignForm.ComponentCount - 1 do
    if not FDesignForm.Components[i].InheritsFrom(TControl) then
      if not IsHasNVC(FDesignForm.Components[i]) then
        BuildDesignNonVisbleComponent(FDesignForm.Components[i]);
  NonCompBringFront;
end;

function TCustomDesigner.GetDesignComponent: TComponent;
begin
//  if FControls.Count = 0 then
//    result  :=  nil
//  else begin
//    Result := TComponent(FControls[0]);
//    if result is TNonVisibleComponentDesgn then
//      result  :=  TNonVisibleComponentDesgn(Result).ContainComponent;
//  end;
end;

procedure TCustomDesigner.setDesignComponent(value: TComponent);
begin
  Select([value]);
  if Assigned(FOnElementSelected) then
    FOnElementSelected(value);
end;

procedure TCustomDesigner.Copy;
begin
  CopySelection;
end;

procedure TCustomDesigner.CopySelection;
var
  S: TMemoryStream;
  W: TWriter;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    W := TWriter.Create(S, 1024);
    try
      W.Root := Root;
      for I := 0 to SelectionCount - 1 do
        if Selections[I] is TComponent then
        begin
          W.WriteSignature;
          W.WriteComponent(TComponent(Selections[I]));
        end;
        W.WriteListEnd;
    finally
      W.Free;
    end;
    CopyStreamToClipboard(S);
  finally
    S.Free;
  end;
end;

procedure TCustomDesigner.Cut;
begin
  CutSelection;
//  Copy;
//  Delete;
//  DesignComponent  :=  FDesignForm;
end;

procedure TCustomDesigner.CutSelection;
begin
  CopySelection;
  DeleteSelection();
end;

procedure TCustomDesigner.Delete;
begin
  DeleteSelection();
end;

procedure TCustomDesigner.DeleteSelection(ADoAll: Boolean);
var
  I: Integer;
  obj: TPersistent;
begin
  for I :=  FSelection.Count-1 downto 0 do
  begin
    obj :=  FSelection.Get(I);
    if obj.InheritsFrom(TComponent) then
      RemoveFromForm(obj as TComponent);
    obj :=  nil;
  end;
  FSelection.ClearSelections;
end;

procedure TCustomDesigner.Paste;
begin
  PasteSelection;
end;

procedure TCustomDesigner.PasteSelection;
var
  S: TStream;
  R: TReader;
begin
  S := GetClipboardStream;
  ClearSelections;
  try
    R := TReader.Create(S, 1024);
    try
      R.OnSetName :=  ReaderSetName;
      R.Root      :=  FDesignForm;
      R.ReadComponents(FDesignForm, GetParent, ComponentRead);
    finally
      NonCompBringFront;
      R.Free;
    end;
  finally
    S.Free;
  end;
end;

function TCustomDesigner.getCanCopyCutOrDelete: boolean;
begin
  result  :=  (SelectCount > 0) and (Selected[0] <> FDesignForm);
end;

function TCustomDesigner.getCanPaste: boolean;
begin
  Result  :=  Clipboard.HasFormat(CF_COMPONENTS) or Clipboard.HasFormat(CF_COMPONENT);
end;

function TCustomDesigner.GetComponent(const Name: string): TComponent;
begin
  Result  :=  FDesignForm.FindComponent(Name);
end;

function TCustomDesigner.GetComponentFromSelect(Selected: TControl): TComponent;
begin
  Result  :=  Selected;
  if Result is TNonVisibleComponentDesgn then
    Result  :=  TNonVisibleComponentDesgn(Result).ContainComponent;
end;

procedure TCustomDesigner.NonCompBringFront;
var
  i: integer;
begin
  for i :=  0 to FNVCHandleManager.ComponentCount - 1 do
    TControl(FNVCHandleManager.Components[i]).BringToFront;
end;

procedure TCustomDesigner.NoSelection;
begin
  ClearSelections;
end;

procedure TCustomDesigner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and (AComponent=FDesignForm) then
    FDesignForm :=  nil;
end;

procedure TCustomDesigner.ShowComponentEditor;
var
  _Selections: TRDesignderSelections;
begin
  try
    _Selections :=  CreateSelection;
    TPopupEditors(FPopupEditors).Edit(_Selections);
  finally
    FreeAndNil(_Selections);
  end;
end;

procedure TCustomDesigner.Edit(const Component: TComponent);
begin
end;

procedure TCustomDesigner.EditComponent(Component: TComponent);
var
  Editor: IComponentEditor;
begin
  Editor  :=  GetComponentEditor(Component, Self);
  if Editor<>nil then
  begin
    Editor.Edit;
    Modified;
  end;
end;

function TCustomDesigner.FindRootAncestor(const AClassName: string): TComponent;
begin

end;

function TCustomDesigner.GetActiveClassGroup: TPersistentClass;
begin
  Result  :=  TPersistentClass(FDesignForm.ClassType);
end;

function TCustomDesigner.GetAncestorDesigner: IDesigner;
begin
  Result  :=  Self;
end;

function TCustomDesigner.GetAppDataDirectory(Local: Boolean): string;
begin
  Result  :=  ExtractFilePath(Application.ExeName);
end;

function TCustomDesigner.GetBaseRegKey: string;
begin
  Result  :=  Self.ConfigFolder;
  if Assigned(FOnGetConfigFolder) then
    FOnGetConfigFolder(Result);
  Result  :=  ExtractFilePath(Application.ExeName)  + Result;
end;

function TCustomDesigner.ControlInSelection(
  Sender: TControl): boolean;
begin
  Result  :=  FSelection.IndexOf(Sender)<>-1;
end;

function TCustomDesigner.getControlByEliminateAssembled(c: TControl): TControl;
  function IsAssembledControl(c: TControl): boolean;  //查看当前选择组件是否是组合型的组件
  var
    i: integer;
  begin
    result  :=  true;
    for i :=  0 to FDesignForm.ComponentCount - 1 do
      if FDesignForm.Components[i] = c then
      begin
        result  :=  false;  //窗体拥有它则不是
        break;
      end;
  end;
begin
  result  :=  c;
  if IsAssembledControl(result) then
    Result  :=  getControlByEliminateAssembled(result.Parent);
end;

procedure TCustomDesigner.CompelOnAfterAddComponent(Adder: TObject);
begin
  if assigned(FOnAfterAddComponent) then
    FOnAfterAddComponent(Adder);
end;

procedure TCustomDesigner.ComponentRead(Component: TComponent);
begin
  AddSelected(Component);
end;

function TCustomDesigner.GetControlByNVCHandleManager(
  Comp: TComponent): TControl;
var
  i: integer;
begin
  Result  :=  nil;
  for i :=  0 to FNVCHandleManager.ComponentCount - 1 do
    if TNonVisibleComponentDesgn(FNVCHandleManager.Components[i]).ContainComponent = Comp then
    begin
      Result  :=  TControl(FNVCHandleManager.Components[i]);
      Break;
    end;
end;

procedure TCustomDesigner.InsertNewComponent(
  CompnentClass: TComponentClass);
var
  Parent: TWinControl;
  p: TPoint;
begin
  if FDesignForm = nil then exit;
  GetCursorPos(P);
  Parent  :=  GetParent;
  P       :=  Parent.ScreenToClient(P);
  CreateComponent(CompnentClass, Parent, P.X, P.Y, -1, -1);
end;

function TCustomDesigner.GetComponentName(Component: TComponent): string;
var
  c: TComponent;
begin
  Result  :=  '';
  if Component<>nil then
  begin
    C       :=  Component;
    Result  :=  C.Name;
    while (c.Owner<>nil) and (c.Owner<>FDesignForm) do
    begin
      Result  :=  '.' + Result;
      C :=  C.Owner;
      Result  :=  C.Name  + Result;
    end;
  end;
  C :=  nil;
end;

procedure TCustomDesigner.GetComponentNames(TypeData: PTypeData;
  Proc: TGetStrProc);

  function SameFrom(CData: PTypeData): boolean;
  begin
    Result  :=  CData = TypeData;
    if not Result and (CData^.ParentInfo<>nil) then
      Result  :=  SameFrom(GetTypeData(CData^.ParentInfo^));
  end;

var
  I: Integer;
  CData: PTypeData;
begin
  if FDesignForm<>nil then
    for I := 0 to FDesignForm.ComponentCount - 1 do
    begin
      CData :=  GetTypeData(FDesignForm.Components[I].ClassInfo);
      if SameFrom(CData) then
        Proc(GetComponentName(FDesignForm.Components[I]));
      CData :=  nil;
    end;
end;

{ THandlePoint }
procedure THandlePoint.ChangeScale(M, D: Integer);
begin
end;

constructor THandlePoint.Create(AManager: TComponent; AControl: TControl; AParent: TWinControl; ADirect: THandlePointDirect);
var
  Rgn: HRGN;
begin
  inherited Create(AManager);
  FPointSize  :=  4;
  FManager    := THandlePointManager(AManager);
  FDesigner   := FManager.FDesigner;
  Color       := clBlack;
  FDirect     := ADirect;
  FControl    := AControl;
  Visible     := False;
  ParentWindow:=  AParent.Handle;
  Pos();
end;

procedure THandlePoint.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := CS_SAVEBITS;
end;

destructor THandlePoint.Destroy;
begin
  inherited Destroy;
end;

function THandlePoint.GeTCustomFormDesigner: TCustomDesigner;
begin
  Result := FManager.FDesigner;
end;

procedure THandlePoint.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if  Designer.Locked then exit;
  if Designer.SelectCount > 1 then Exit;
  Designer.Dragging := True;
  Designer.FBeforDragPos := ClientToScreen(Point(X, Y));
  MouseCapture := True;
end;

procedure THandlePoint.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I                 : Integer;
  CPos              : TPoint;
  cX, cY            : Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if  Designer.Locked then exit;
  if not Designer.Dragging then  Exit;
  CPos := ClientToScreen(Point(X, Y));

  cX := Designer.FBeforDragPos.X - CPos.X;
  cY := Designer.FBeforDragPos.Y - CPos.Y;
  if (Abs(cX) < 2) and (Abs(cY) < 2) then
    Exit;

  case FDirect of
    hpdLeftUp:
      begin
        if FControl.Width + cX > 1 then
        begin
          FControl.Left := FControl.Left - cX;
          FControl.Width := FControl.Width + cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
        if FControl.Height + cY > 1 then
        begin
          FControl.Top := FControl.Top - cY;
          FControl.Height := FControl.Height + cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdUp:
      begin
        if FControl.Height + cY > 1 then
        begin
          FControl.Top := FControl.Top - cY;
          FControl.Height := FControl.Height + cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdRightUp:
      begin
        if FControl.Width - cX > 1 then
        begin
          FControl.Width := FControl.Width - cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
        if FControl.Height + cY > 1 then
        begin
          FControl.Top := FControl.Top - cY;
          FControl.Height := FControl.Height + cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdRight:
      begin
        if FControl.Width - cX > 1 then
        begin
          FControl.Width := FControl.Width - cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
      end;
    hpdRightDown:
      begin
        if FControl.Width - cX > 1 then
        begin
          FControl.Width := FControl.Width - cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
        if FControl.Height - cY > 1 then
        begin
          FControl.Height := FControl.Height - cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdDown:
      begin
        if FControl.Height - cY > 1 then
        begin
          FControl.Height := FControl.Height - cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdLeftDown:
      begin
        if FControl.Width + cX > 1 then
        begin
          FControl.Left := FControl.Left - cX;
          FControl.Width := FControl.Width + cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
        if FControl.Height - cY > 1 then
        begin
          FControl.Height := FControl.Height - cY;
          Designer.FBeforDragPos.Y := CPos.Y;
        end;
      end;
    hpdLeft:
      begin
        if FControl.Width + cX > 1 then
        begin
          FControl.Left := FControl.Left - cX;
          FControl.Width := FControl.Width + cX;
          Designer.FBeforDragPos.X := CPos.X;
        end;
      end;
  end;

end;

procedure THandlePoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
  Designer.Dragging := False;
end;

procedure THandlePoint.Paint;
begin
  inherited;
  Canvas.Brush.Color  :=  Color;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color  :=  clBlack;
  Canvas.Rectangle(ClientRect);
end;

procedure THandlePoint.Pos();
var
  X: array[0..2] of Integer;
  Y: array[0..2] of Integer;
begin
  X[0] := FControl.Left - FPointSize div 2;
  X[1] := FControl.Left + (FControl.Width - FPointSize) div 2;
  X[2] := FControl.Left + FControl.Width - FPointSize div 2;
  Y[0] := FControl.Top - FPointSize div 2;
  Y[1] := FControl.Top + (FControl.Height - FPointSize) div 2;
  Y[2] := FControl.Top + FControl.Height - FPointSize div 2;
  case FDirect of
    hpdLeftUp:
      begin
        Cursor := crSizeNWSE;
        SetBounds(X[0], Y[0], FPointSize, FPointSize);
      end;
    hpdUp:
      begin
        Cursor := crSizeNS;
        SetBounds(X[1], Y[0], FPointSize, FPointSize);
      end;
    hpdRightUp:
      begin
        Cursor := crSizeNESW;
        SetBounds(X[2], Y[0], FPointSize, FPointSize);
      end;
    hpdRight:
      begin
        Cursor := crSizeWE;
        SetBounds(X[2], Y[1], FPointSize, FPointSize);
      end;
    hpdRightDown:
      begin
        Cursor := crSizeNWSE;
        SetBounds(X[2], Y[2], FPointSize, FPointSize);
      end;
    hpdDown:
      begin
        Cursor := crSizeNS;
        SetBounds(X[1], Y[2], FPointSize, FPointSize);
      end;
    hpdLeftDown:
      begin
        Cursor := crSizeNESW;
        SetBounds(X[0], Y[2], FPointSize, FPointSize);
      end;
    hpdLeft:
      begin
        Cursor := crSizeWE;
        SetBounds(X[0], Y[1], FPointSize, FPointSize);
      end;
  end;
  if (Designer.Locked) or (FDesigner.SelectCount > 1) then Cursor := crDefault;
  BringToFront;
end;

procedure THandlePoint.WMEraseBkgnd(var Message: TWMErasebkgnd);
begin
end;

{ THandlePointManager }

constructor THandlePointManager.Create(ADesigner: TCustomDesigner);
begin
  inherited Create(nil);
  FDesigner := ADesigner;
end;

{ TNVCHandleManager }

constructor TNVCHandleManager.Create(ADesigner: TCustomDesigner);
begin
  inherited Create(nil);
  FDesigner := ADesigner;
end;

procedure TNVCHandleManager.UpdateComponentName(Component: TComponent);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if TNonVisibleComponentDesgn(Components[I]).ContainComponent=Component then
    begin
      TNonVisibleComponentDesgn(Components[I]).UpdateComponentName;
      Break;
    end;
end;

{ TRDesignderSelections }
procedure TRDesignderSelections.Delete(Item: TPersistent);
var
  Idx: Integer;
begin
  Idx :=  IndexOf(Item);
  if Idx<>-1 then
    Delete(Idx);
end;

function TRDesignderSelections.Add(const Item: TPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

procedure TRDesignderSelections.ClearSelections;
begin
  FList.Clear;
end;

constructor TRDesignderSelections.Copy(const Selections: IDesignerSelections);
var
  I: Integer;
begin
  Create;
  for I := 0 to Selections.Count - 1 do
    Add(Selections[I]);
end;

constructor TRDesignderSelections.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TRDesignderSelections.Delete(const Index: Integer);
begin
  if (Index>-1) and (Index<FList.Count) then
    FList.Delete(Index);
end;

destructor TRDesignderSelections.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRDesignderSelections.Equals(const List: IDesignerSelections): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List.Count <> Count then Exit;
  for I := 0 to Count - 1 do
    if Items[I] <> List[I] then Exit;
  Result := True;
end;

function TRDesignderSelections.Get(Index: Integer): TPersistent;
begin
  Result := TPersistent(FList[Index]);
end;

function TRDesignderSelections.GetCount: Integer;
begin
  Result  :=  FList.Count;
end;

function TRDesignderSelections.GetDesignObject(Index: Integer): IDesignObject;
begin
  Result  :=  PersistentToDesignObject(Get(Index));
end;

function TRDesignderSelections.IndexOf(const Value: TPersistent): Integer;
var
  I: Integer;
begin
  for I := 0 to self.Count - 1 do
    if Get(I)=Value then
    begin
      Result  :=  I;
      Exit;
    end;
  Result  :=  -1;//FList.IndexOf(Value);
end;

procedure DoRegisterDesignNotification(const DesignNotification: IDesignNotification);
begin
  DesignNotificationMgr.RegisterDesignNotification(DesignNotification);
end;

procedure DoUnRegisterDesignNotification(const DesignNotification: IDesignNotification);
begin
  DesignNotificationMgr.UnRegisterDesignNotification(DesignNotification);
end;

{ DesignNotificationManager }

procedure DesignNotificationManager.PostDesignerClosed(
  const ADesigner: IDesigner; AGoingDormant: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.DesignerClosed(ADesigner, AGoingDormant);
end;

procedure DesignNotificationManager.PostDesignerOpened(
  const ADesigner: IDesigner; AResurrecting: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.DesignerOpened(ADesigner, AResurrecting);
end;

procedure DesignNotificationManager.PostItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.ItemDeleted(ADesigner, AItem);
end;

procedure DesignNotificationManager.PostItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.ItemInserted(ADesigner, AItem);
end;

procedure DesignNotificationManager.PostItemsModified(
  const ADesigner: IDesigner);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.ItemsModified(ADesigner);
end;

procedure DesignNotificationManager.PostSelectionChanged(
  const ADesigner: IDesigner; const ASelection: IDesignerSelections);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PDesignNotificationRec(Items[I])^.DesignNotification.SelectionChanged(ADesigner, ASelection);
end;

procedure DesignNotificationManager.RegisterDesignNotification(
  const DesignNotification: IDesignNotification);
var
  p:  PDesignNotificationRec;
begin
  new(p);
  p^.DesignNotification :=  DesignNotification;
  Add(p);
end;

procedure DesignNotificationManager.UnRegisterDesignNotification(
  const DesignNotification: IDesignNotification);
var
  p:  PDesignNotificationRec;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    p :=  PDesignNotificationRec(Items[I]);
    if p^.DesignNotification=DesignNotification then
    begin
      Delete(I);
      FreeMem(p);
      Break;
    end;
  end;
end;

initialization
  DesignNotificationMgr :=  DesignNotificationManager.Create;
  //确定不是依附于Delphi的IDE的情况下,处理下述过程
  if ISRunTime then
  begin
    RegisterDesignNotificationProc   :=  DoRegisterDesignNotification;
    UnregisterDesignNotificationProc :=  DoUnRegisterDesignNotification;

    RegisterPropertyEditor(TypeInfo(String), nil, '', TStringProperty);
    RegisterPropertyEditor(TypeInfo(Integer), nil, '', TIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), nil, '', TBooleanProperty);
    RegisterPropertyEditor(TypeInfo(Int64), nil, '', TInt64Property);
    RegisterPropertyEditor(TypeInfo(Double), nil, '', TFloatProperty);
    RegisterPropertyEditor(TypeInfo(TClass), nil, '', TClassProperty);
    RegisterPropertyEditor(TypeInfo(TMethod), nil, '', TMethodProperty);
    {$IFDEF UNICODE}
    RegisterPropertyEditor(TypeInfo(WideString), nil, '', TWideStringProperty);
    RegisterPropertyEditor(TypeInfo(WideChar), nil, '', TWideCharProperty);
    {$ENDIF}
    RegisterPropertyEditor(TypeInfo(TComponent), nil, '', TComponentProperty);
    RegisterPropertyEditor(TypeInfo(IInterface), nil, '', TInterfaceProperty);
    RegisterPropertyEditor(TypeInfo(TCaption), nil, '', TCaptionProperty);
    RegisterPropertyEditor(TypeInfo(TComponentName), nil, '', TComponentNameProperty);
    RegisterPropertyEditor(TypeInfo(TDate), nil, '', TDateProperty);
    RegisterPropertyEditor(TypeInfo(TTime), nil, '', TTimeProperty);
    RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TDateTimeProperty);
    RegisterPropertyEditor(TypeInfo(Variant), nil, '', TVariantProperty);
    RegisterPropertyEditor(TypeInfo(TFontName), nil, '', TFontNameProperty);
    RegisterPropertyEditor(TypeInfo(TFontCharset), nil, '', TFontCharsetProperty);
    RegisterPropertyEditor(TypeInfo(TImeName), nil, '', TImeNameProperty);
    RegisterPropertyEditor(TypeInfo(TFont), nil, '', TFontProperty);
    RegisterPropertyEditor(TypeInfo(TModalResult), nil, '', TModalResultProperty);
    RegisterPropertyEditor(TypeInfo(TShortCut), nil, '', TShortCutProperty);
    RegisterPropertyEditor(TypeInfo(TPicture), nil, '', uPicEdit.TPictureProperty);
    RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', uPicEdit.TGraphicProperty);
    RegisterPropertyEditor(TypeInfo(TColor), nil, '', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TCursor), nil, '', TCursorProperty);
    RegisterPropertyEditor(TypeInfo(TMenuItem), TCustomForm, 'WindowMenu', TComponentProperty);

    RegisterSelectionEditor(TComponent, TSelectionEditor);
  end;

finalization
  FreeAndNil(DesignNotificationMgr);
  if ISRunTime then
  begin
    RegisterDesignNotificationProc   :=  nil;
    UnregisterDesignNotificationProc :=  nil;
  end;

end.


