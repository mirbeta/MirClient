{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxGridStdPopupMenu;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, ImgList, Menus, cxGridMenuOperations, cxGridCustomPopupMenu, cxGridCustomView,
  dxBuiltInPopupMenu;

type

  { TcxGridPopupMenuItem }

  TcxGridPopupMenuItem = class(TMenuItem)
  strict private
    FOperation: TcxGridPopupMenuOperation;
  public
    constructor Create(AOwner: TComponent; AOperation: TcxGridPopupMenuOperation); reintroduce; virtual;
    //
    property Operation: TcxGridPopupMenuOperation read FOperation;
  end;

  { TcxGridStdPopupMenu }

  TcxGridStdPopupMenu = class(TPopupMenu, IDoPopup, IcxGridPopupMenu)
  strict private
    FOperations: TcxGridPopupMenuOperations;

    procedure AdapterClickHandler(Sender: TObject);
    procedure PopupViaAdapter(const P: TPoint; AAdapterClass: TdxCustomBuiltInPopupMenuAdapterClass);
  protected
    // IDoPopup
    function GetPopupHandler: TcxGridOnPopupProc;
    // IcxGridPopupMenu
    function CanPopup(AHitTest: TcxCustomGridHitTest): Boolean;

    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState); virtual;
    procedure CreateMenuItems; virtual;
    procedure DoPopup(Sender: TObject); override;
    procedure GridMenuPopup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X,Y: Integer); virtual;
    function HasDynamicContent: Boolean;
    procedure InitItems(AItems: TMenuItem);
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer); virtual;

    function GetOperationsClass: TcxGridPopupMenuOperationsClass; virtual; abstract;
    property Operations: TcxGridPopupMenuOperations read FOperations;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X, Y: Integer); override;
  end;

  { TcxGridStdHeaderMenu }

  TcxGridStdHeaderMenu = class(TcxGridStdPopupMenu)
  protected
    function GetOperationsClass: TcxGridPopupMenuOperationsClass; override;
  end;

  { TcxGridStdGroupByBoxMenu }

  TcxGridStdGroupByBoxMenu = class(TcxGridStdPopupMenu)
  protected
    function GetOperationsClass: TcxGridPopupMenuOperationsClass; override;
  end;

  { TcxGridStdFooterMenu }

  TcxGridStdFooterMenu = class(TcxGridStdPopupMenu)
  protected
    function GetOperationsClass: TcxGridPopupMenuOperationsClass; override;
  end;

  { TcxGridStdGroupRowMenu }

  TcxGridStdGroupRowMenu = class(TcxGridStdPopupMenu)
  protected
    function GetOperationsClass: TcxGridPopupMenuOperationsClass; override;
    procedure GridMenuPopup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X,Y: Integer); override;
  end;

  { TcxGridStdDataRowMenu }

  TcxGridStdDataRowMenu = class(TcxGridStdPopupMenu)
  protected
    function GetOperationsClass: TcxGridPopupMenuOperationsClass; override;
  end;

function cxGetGridPopupMenuOperation(AMenuItem: TComponent): TcxGridPopupMenuOperation;
implementation

uses
  Types, Themes, SysUtils, cxGraphics, cxGeometry, cxLookAndFeelPainters,
  cxGridCustomTableView, cxGridTableView, cxGridHeaderPopupMenuItems, cxGridDataRowPopupMenuItems,
  cxGridFooterPopupMenuItems, cxGridGroupRowPopupMenuItems, cxGridGroupByBoxPopupMenuItems;

function cxGetGridPopupMenuOperation(AMenuItem: TComponent): TcxGridPopupMenuOperation;
begin
  if AMenuItem is TcxGridPopupMenuItem then
    Result := TcxGridPopupMenuItem(AMenuItem).Operation
  else
    Result := nil;
end;

{ TcxGridPopupMenuItem }

constructor TcxGridPopupMenuItem.Create(AOwner: TComponent; AOperation: TcxGridPopupMenuOperation);
begin
  inherited Create(AOwner);
  FOperation := AOperation;
end;

{ TcxGridStdPopupMenu }

constructor TcxGridStdPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FOperations := GetOperationsClass.Create;
  if not HasDynamicContent then
    CreateMenuItems;
end;

destructor TcxGridStdPopupMenu.Destroy;
begin
  FreeAndNil(FOperations);
  inherited;
end;

function TcxGridStdPopupMenu.GetPopupHandler: TcxGridOnPopupProc;
begin
  Result := GridMenuPopup;
end;

function TcxGridStdPopupMenu.CanPopup(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := Operations.CanProcess(AHitTest);
end;

procedure TcxGridStdPopupMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
begin
  cxAdvancedDrawPopupMenuItem(TcxGridPopupMenuItem(Sender), ACanvas, ARect, State);
end;

procedure TcxGridStdPopupMenu.CreateMenuItems;

  procedure InsertNewGroup(AItems: TMenuItem);
  var
    AItem: TcxGridPopupMenuItem;
  begin
    AItem := TcxGridPopupMenuItem.Create(Self, nil);
    AItem.Caption := '-';
    AItems.Add(AItem);
  end;

  function GetMenuItemByOperationClass(const AItems: TMenuItem; AClass: TcxGridPopupMenuOperationClass): TMenuItem;
  var
    I: Integer;
    AOperation: TcxGridPopupMenuOperation;
  begin
    for I := 0 to AItems.Count - 1 do
    begin
      Result := AItems[I];
      AOperation := cxGetGridPopupMenuOperation(Result);
      if (AOperation <> nil) and (AOperation.ClassType = AClass) then
        Exit;
      Result := GetMenuItemByOperationClass(Result, AClass);
      if Result <> nil then
        Exit;
    end;
    Result := nil;
  end;

var
  I: Integer;
  AOperation: TcxGridPopupMenuOperation;
  AMenuItem, AParentMenuItem: TMenuItem;
begin
  for I := 0 to Operations.Count - 1 do
  begin
    AOperation := Operations[I];
    AMenuItem := TcxGridPopupMenuItem.Create(Self, AOperation);

    if AOperation.GetParentOperationClass = nil then
      AParentMenuItem := nil
    else
      AParentMenuItem := GetMenuItemByOperationClass(Items, AOperation.GetParentOperationClass);
    if AParentMenuItem = nil then
      AParentMenuItem := Items;

    if AOperation.BeginGroup then
      InsertNewGroup(AParentMenuItem);
    AParentMenuItem.Add(AMenuItem);
  end;
end;

procedure TcxGridStdPopupMenu.DoPopup(Sender: TObject);
begin
  if HasDynamicContent then
  begin
    Items.Clear;
    CreateMenuItems;
  end;
  Images := cxGridPopupMenuImages;
  InitItems(Items);
  inherited DoPopup(Sender);
end;

procedure TcxGridStdPopupMenu.GridMenuPopup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
begin
  Popup(X, Y);
end;

function TcxGridStdPopupMenu.HasDynamicContent: Boolean;
begin
  Result := Operations.HasDynamicContent;
end;

procedure TcxGridStdPopupMenu.InitItems(AItems: TMenuItem);
var
  ACustomDrawNeeded: Boolean;
  I: Integer;
  AOperation: TcxGridPopupMenuOperation;
begin
  ACustomDrawNeeded := (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) and (Images <> nil) and not IsRightToLeft;
  for I := 0 to AItems.Count - 1 do
  begin
    AOperation := cxGetGridPopupMenuOperation(AItems[I]);
    if AOperation <> nil then
    begin
      AItems[I].Caption := AOperation.Caption;
      AItems[I].Checked := AOperation.Down;
      AItems[I].Enabled := AOperation.Enabled;
      AItems[I].ImageIndex := AOperation.ImageIndex;
      AItems[I].Visible := AOperation.Visible;
      AItems[I].OnClick := AOperation.DoExecute;
      if ACustomDrawNeeded then
        AItems[I].OnAdvancedDrawItem := AdvancedDrawItem;
      AItems[I].OnMeasureItem := MeasureItem;
    end;
    InitItems(AItems[I]);
  end;
end;

procedure TcxGridStdPopupMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
begin
  dxAdjustToTouchableSize(Height);
end;

procedure TcxGridStdPopupMenu.Popup(X, Y: Integer);
begin
  if TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    inherited
  else
    PopupViaAdapter(Point(X, Y), TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass);
end;

procedure TcxGridStdPopupMenu.AdapterClickHandler(Sender: TObject);
begin
  TMenuItem((Sender as TComponent).Tag).Click;
end;

procedure TcxGridStdPopupMenu.PopupViaAdapter(const P: TPoint; AAdapterClass: TdxCustomBuiltInPopupMenuAdapterClass);
var
  AAdapter: TdxCustomBuiltInPopupMenuAdapter;
begin
  AAdapter := AAdapterClass.Create(PopupComponent);
  try
    DoPopup(Self);
    TdxBuiltInPopupMenuAdapterHelper.AddMenu(AAdapter, Self, AdapterClickHandler);
    AAdapter.Popup(P);
  finally
    AAdapter.Free;
  end;
end;

{ TcxGridStdHeaderMenu }

function TcxGridStdHeaderMenu.GetOperationsClass: TcxGridPopupMenuOperationsClass;
begin
  Result := TcxGridHeaderPopupMenuOperations;
end;

{ TcxGridStdFindPanelMenu }

function TcxGridStdGroupByBoxMenu.GetOperationsClass: TcxGridPopupMenuOperationsClass;
begin
  Result := TcxGridGroupByBoxPopupMenuOperations;
end;

{ TcxGridStdFooterMenu }

function TcxGridStdFooterMenu.GetOperationsClass: TcxGridPopupMenuOperationsClass;
begin
  Result := TcxGridFooterPopupMenuOperations;
end;

{ TcxGridStdGroupRowMenu }

function TcxGridStdGroupRowMenu.GetOperationsClass: TcxGridPopupMenuOperationsClass;
begin
  Result := TcxGridGroupRowPopupMenuOperations;
end;

procedure TcxGridStdGroupRowMenu.GridMenuPopup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
begin
  TcxGridGroupRowPopupMenuOperations(Operations).GroupRow :=
    (AHitTest as TcxGridRecordHitTest).GridRecord as TcxGridGroupRow;
  inherited;
end;

{ TcxGridStdDataRowMenu }

function TcxGridStdDataRowMenu.GetOperationsClass: TcxGridPopupMenuOperationsClass;
begin
  Result := TcxGridDataRowPopupMenuOperations;
end;

initialization
  RegisterPopupMenuClass(TcxGridStdHeaderMenu, [gvhtColumnHeader], TcxGridTableView);
  RegisterPopupMenuClass(TcxGridStdGroupByBoxMenu, [gvhtGroupByBox], TcxCustomGridTableView);
  RegisterPopupMenuClass(TcxGridStdFooterMenu, [gvhtFooterCell, gvhtGroupFooterCell], TcxGridTableView);
  RegisterPopupMenuClass(TcxGridStdGroupRowMenu, [gvhtRecord, gvhtGroupSummary], TcxGridTableView);
  RegisterPopupMenuClass(TcxGridStdDataRowMenu, [gvhtRecord, gvhtCell], TcxGridTableView);

finalization
  UnregisterPopupMenuClass(TcxGridStdDataRowMenu);
  UnregisterPopupMenuClass(TcxGridStdGroupRowMenu);
  UnregisterPopupMenuClass(TcxGridStdFooterMenu);
  UnregisterPopupMenuClass(TcxGridStdGroupByBoxMenu);
  UnregisterPopupMenuClass(TcxGridStdHeaderMenu);
end.

