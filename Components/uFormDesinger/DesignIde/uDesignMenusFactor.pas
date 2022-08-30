unit uDesignMenusFactor;

interface
  uses Classes, Sysutils, uDesignMenus, Menus;

type
  TMenuItemFactor = class(TInterfacedObject, IMenuItem)
  private
    FMenuItem: TMenuItem;
  protected
    {IMenuItems}
    function GetItem(Index: Integer): IMenuItem;
    function SameAs(const AItem: IUnknown): Boolean;
    function Find(const ACaption: WideString): IMenuItem;
    function FindByName(const AName: string): IMenuItem;
    function Count: Integer;
    property Items[Index: Integer]: IMenuItem read GetItem;
    procedure Clear;

    function AddItem(const ACaption: WideString; AShortCut: TShortCut;
      AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent = nil;
      hCtx: THelpContext = 0; const AName: string = ''): IMenuItem; overload;

    function AddItem(AAction: TBasicAction;
      const AName: string = ''): IMenuItem; overload;

    function InsertItem(const ACaption: WideString;
      AShortCut: TShortCut; AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent = nil;
      hCtx: THelpContext = 0; const AName: string = ''): IMenuItem; overload;
    function InsertItem(Index: Integer; const ACaption: WideString;
      AShortCut: TShortCut; AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent = nil;
      hCtx: THelpContext = 0; const AName: string = ''): IMenuItem; overload;

    function InsertItem(AAction: TBasicAction;
      const AName: string = ''): IMenuItem; overload;
    function InsertItem(Index: Integer; AAction: TBasicAction;
      const AName: string = ''): IMenuItem; overload;

    function AddLine(const AName: string = ''): IMenuItem;

    function InsertLine(const AName: string = ''): IMenuItem; overload;
    function InsertLine(Index: Integer; const AName: string = ''): IMenuItem; overload;
    {IMenuItem}
    function GetCaption: WideString;
    procedure SetCaption(const ACaption: WideString);
    function GetChecked: Boolean;
    procedure SetChecked(AChecked: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    function GetGroupIndex: Byte;
    procedure SetGroupIndex(AGroupIndex: Byte);
    function GetHelpContext: THelpContext;
    procedure SetHelpContext(AHelpContext: THelpContext);
    function GetHint: string;
    procedure SetHint(const AHint: string);
    function GetRadioItem: Boolean;
    procedure SetRadioItem(ARadioItem: Boolean);
    function GetShortCut: TShortCut;
    procedure SetShortCut(AShortCut: TShortCut);
    function GetTag: LongInt;
    procedure SetTag(AValue: LongInt);
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);

    // public
    function Name: TComponentName;
    function MenuIndex: Integer;
    function Parent: IMenuItem;
    function HasParent: Boolean;
    function IsLine: Boolean;
  public
    constructor Create(AMenuItem: TMenuItem);
    destructor Destroy; override;
  end;

implementation

{ TMenuItemFactor }

function TMenuItemFactor.AddItem(const ACaption: WideString;
  AShortCut: TShortCut; AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent;
  hCtx: THelpContext; const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Caption  :=  ACaption;
  item.ShortCut :=  AShortCut;
  item.Checked  :=  AChecked;
  item.Enabled  :=  AEnabled;
  item.OnClick  :=  AOnClick;
  item.Name     :=  AName;
  item.HelpContext  :=  hCtx;
  FMenuItem.Add(item);
  Result  :=  TMenuItemFactor.Create(item);
end;

function TMenuItemFactor.AddItem(AAction: TBasicAction;
  const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Name     :=  AName;
  item.Action   :=  AAction;
  FMenuItem.Add(item);
  Result  :=  TMenuItemFactor.Create(item);
end;

function TMenuItemFactor.AddLine(const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Name     :=  AName;
  item.Caption  :=  '-';
  FMenuItem.Add(item);
  Result  :=  TMenuItemFactor.Create(item);
end;

procedure TMenuItemFactor.Clear;
begin
  FMenuItem.Clear;
end;

function TMenuItemFactor.Count: Integer;
begin
  Result  :=  FMenuItem.Count;
end;

constructor TMenuItemFactor.Create(AMenuItem: TMenuItem);
begin
  FMenuItem :=  AMenuItem;
end;

destructor TMenuItemFactor.Destroy;
begin
  FMenuItem :=  nil;
  inherited;
end;

function TMenuItemFactor.Find(const ACaption: WideString): IMenuItem;
var
  I: Integer;
begin
  for I := 0 to FMenuItem.Count - 1 do
    if SameText(ACaption, FMenuItem.Items[I].Caption) then
    begin
      Result  :=  TMenuItemFactor.Create(FMenuItem.Items[I]);
      Exit;
    end;
  Result  :=  nil;
end;

function TMenuItemFactor.FindByName(const AName: string): IMenuItem;
var
  I: Integer;
begin
  for I := 0 to FMenuItem.Count - 1 do
    if SameText(AName, FMenuItem.Items[I].Name) then
    begin
      Result  :=  TMenuItemFactor.Create(FMenuItem.Items[I]);
      Exit;
    end;
  Result  :=  nil;
end;

function TMenuItemFactor.GetCaption: WideString;
begin
  Result  :=  FMenuItem.Caption;
end;

function TMenuItemFactor.GetChecked: Boolean;
begin
  Result  :=  FMenuItem.Checked;
end;

function TMenuItemFactor.GetEnabled: Boolean;
begin
  Result  :=  FMenuItem.Enabled;
end;

function TMenuItemFactor.GetGroupIndex: Byte;
begin
  Result  :=  FMenuItem.GroupIndex;
end;

function TMenuItemFactor.GetHelpContext: THelpContext;
begin
  Result  :=  FMenuItem.HelpContext;
end;

function TMenuItemFactor.GetHint: string;
begin
  Result  :=  FMenuItem.Hint;
end;

function TMenuItemFactor.GetItem(Index: Integer): IMenuItem;
begin
  Result  :=  TMenuItemFactor.Create(FMenuItem.Items[Index]);
end;

function TMenuItemFactor.GetRadioItem: Boolean;
begin
  Result  :=  FMenuItem.RadioItem;
end;

function TMenuItemFactor.GetShortCut: TShortCut;
begin
  Result  :=  FMenuItem.ShortCut;
end;

function TMenuItemFactor.GetTag: LongInt;
begin
  Result  :=  FMenuItem.Tag;
end;

function TMenuItemFactor.GetVisible: Boolean;
begin
  Result  :=  FMenuItem.Visible;
end;

function TMenuItemFactor.HasParent: Boolean;
begin
  Result  :=  FMenuItem.HasParent;
end;

function TMenuItemFactor.InsertItem(const ACaption: WideString;
  AShortCut: TShortCut; AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent;
  hCtx: THelpContext; const AName: string): IMenuItem;
begin
  Result  :=  InsertItem(FMenuItem.Count, ACaption, AShortCut, AChecked, AEnabled,
    AOnClick, hCtx, AName);
end;

function TMenuItemFactor.InsertItem(AAction: TBasicAction;
  const AName: string): IMenuItem;
begin
  Result  :=  InsertItem(FMenuItem.Count, AAction, AName);
end;

function TMenuItemFactor.InsertItem(Index: Integer; AAction: TBasicAction;
  const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Action   :=  AAction;
  item.Name     :=  AName;
  FMenuItem.Insert(Index, item);
  Result  :=  TMenuItemFactor.Create(item);
end;

function TMenuItemFactor.InsertItem(Index: Integer; const ACaption: WideString;
  AShortCut: TShortCut; AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent;
  hCtx: THelpContext; const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Caption  :=  ACaption;
  item.ShortCut :=  AShortCut;
  item.Checked  :=  AChecked;
  item.Enabled  :=  AEnabled;
  item.OnClick  :=  AOnClick;
  item.Name     :=  AName;
  item.HelpContext  :=  hCtx;
  FMenuItem.Insert(Index, item);
  Result  :=  TMenuItemFactor.Create(item);
end;

function TMenuItemFactor.InsertLine(Index: Integer;
  const AName: string): IMenuItem;
var
  item: TMenuItem;
begin
  item  :=  TMenuItem.Create(FMenuItem.Owner);
  item.Caption  :=  '-';
  item.Name     :=  AName;
  FMenuItem.Insert(Index, item);
  Result  :=  TMenuItemFactor.Create(item);
end;

function TMenuItemFactor.InsertLine(const AName: string): IMenuItem;
begin
  Result  :=  InsertLine(FMenuItem.Count, AName);
end;

function TMenuItemFactor.IsLine: Boolean;
begin
  Result  :=  FMenuItem.IsLine;
end;

function TMenuItemFactor.MenuIndex: Integer;
begin
  Result  :=  FMenuItem.MenuIndex;
end;

function TMenuItemFactor.Name: TComponentName;
begin
  Result  :=  FMenuItem.Name;
end;

function TMenuItemFactor.Parent: IMenuItem;
begin
  Result  :=  TMenuItemFactor.Create(FMenuItem.Parent);
end;

function TMenuItemFactor.SameAs(const AItem: IInterface): Boolean;
begin

end;

procedure TMenuItemFactor.SetCaption(const ACaption: WideString);
begin
  FMenuItem.Caption :=  ACaption;
end;

procedure TMenuItemFactor.SetChecked(AChecked: Boolean);
begin
  FMenuItem.Checked :=  AChecked;
end;

procedure TMenuItemFactor.SetEnabled(AEnabled: Boolean);
begin
  FMenuItem.Enabled :=  AEnabled;
end;

procedure TMenuItemFactor.SetGroupIndex(AGroupIndex: Byte);
begin
  FMenuItem.GroupIndex  :=  AGroupIndex;
end;

procedure TMenuItemFactor.SetHelpContext(AHelpContext: THelpContext);
begin
  FMenuItem.HelpContext :=  AHelpContext;
end;

procedure TMenuItemFactor.SetHint(const AHint: string);
begin
  FMenuItem.Hint  :=  AHint;
end;

procedure TMenuItemFactor.SetRadioItem(ARadioItem: Boolean);
begin
  FMenuItem.RadioItem :=  ARadioItem;
end;

procedure TMenuItemFactor.SetShortCut(AShortCut: TShortCut);
begin
  FMenuItem.ShortCut  :=  AShortCut;
end;

procedure TMenuItemFactor.SetTag(AValue: Integer);
begin
  FMenuItem.Tag :=  AValue;
end;

procedure TMenuItemFactor.SetVisible(AVisible: Boolean);
begin
  FMenuItem.Visible :=  AVisible;
end;

end.
