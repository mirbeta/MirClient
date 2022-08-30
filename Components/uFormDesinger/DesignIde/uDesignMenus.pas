{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uDesignMenus;

interface

uses
  Types, Classes;

type
  IMenuItem = interface;

  IMenuItems = interface
    ['{C9CC6C38-C96A-4514-8D6F-1D121727BFAF}']

    // protected
    function GetItem(Index: Integer): IMenuItem;

    // public
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
  end;

  IMenu = interface
    ['{0993FAE4-17E2-4EB7-81DF-26634D7F9E16}']
    function Items: IMenuItems;
  end;

  IMainMenu = interface(IMenu)
    ['{5D137DC1-73F4-48CB-8351-E14A369AE924}']
  end;

  IPopupMenu = interface(IMenu)
    ['{E2E9ED8C-4D54-482B-AC62-23F1CEBFE414}']

    procedure Popup(X, Y: Integer);
    function PopupComponent: TComponent;
  end;

  IMenuItem = interface(IMenuItems)
    ['{DAF029E1-9592-4B07-A450-A10056A2B9B5}']

    // protected
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

    property Caption: WideString read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property GroupIndex: Byte read GetGroupIndex write SetGroupIndex;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext;
    property Hint: string read GetHint write SetHint;
    property RadioItem: Boolean read GetRadioItem write SetRadioItem;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property Tag: LongInt read GetTag write SetTag;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IMenuItem2 = interface(IMenuItem)
    ['{6F462D74-C6F5-473C-AFE1-42564CC71B9F}']
    procedure Click;
  end;

type
  TLocalMenuType = (lmBase, lmModule, lmSelection, lmComponent);
  TLocalMenuTypes = set of TLocalMenuType;

const
  CNoLocalMenus = [];
  CAllLocalMenus = [Low(TLocalMenuType)..High(TLocalMenuType)];
  CLocalMenusIf: array [Boolean] of TLocalMenuTypes =
    (CNoLocalMenus, CAllLocalMenus);

type
  IDesignLocalMenu140 = interface
    ['{70ED1A8D-6275-4BC8-813C-F6D9066FD6BB}']
    function BuildLocalMenu(Allow: TLocalMenuTypes = CAllLocalMenus): IPopupMenu; overload;
    procedure InvokeLocalMenu(X, Y: Integer);
  end;

  IDesignLocalMenu = interface(IDesignLocalMenu140)
    ['{22A561B0-21FF-43F7-BF53-C23C6028ABD1}']
    function BuildLocalMenu(const Selections: IInterface; Allow: TLocalMenuTypes = CAllLocalMenus): IPopupMenu; overload;
  end;

implementation

end.
