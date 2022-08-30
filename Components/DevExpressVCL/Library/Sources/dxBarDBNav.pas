{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars DB Navigator                                 }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarDBNav;

interface

{$I cxVer.inc}

uses
  Classes, Messages, Controls, DB, dxCore, dxCoreClasses, cxClasses, dxBar;

type
  TdxBarDBNavigator = class;
  TdxBarDBNavButton = class;

  TdxBarDBNavDataLink = class(TDataLink)
  private
    FNavigator: TdxBarDBNavigator;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure EditingChanged; override;
  end;

  TdxBarDBEnableType = (dxdbtCanModify, dxdbtNotEOF, dxdbtNotBOF,
    dxdbtHasRecords, dxdbtIsModified, dxdbtIsNotModified);
  TdxBarDBEnableTypes = set of TdxBarDBEnableType;

  TdxBarDBCheckLink = class(TCollectionItem)
  private
    FEnableTypes: TdxBarDBEnableTypes;
    FFreeNotificator: TcxFreeNotificator;
    FItem: TdxBarItem;
    procedure FreeNotification(Sender: TComponent);
    function GetDBNavigator: TdxBarDBNavigator;
    procedure SetEnableTypes(Value: TdxBarDBEnableTypes);
    procedure SetItem(Value: TdxBarItem);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Value: TPersistent); override;
    property DBNavigator: TdxBarDBNavigator read GetDBNavigator;
  published
    property Item: TdxBarItem read FItem write SetItem;
    property EnableTypes: TdxBarDBEnableTypes read FEnableTypes write SetEnableTypes;
  end;

  TdxBarDBCheckLinks = class(TCollection)
  private
    FDBNavigator: TdxBarDBNavigator;
    function GetItem(Index: Integer): TdxBarDBCheckLink;
  protected
    function GetOwner: TPersistent; override;
  public
    function Add: TdxBarDBCheckLink;
    property Items[Index: Integer]: TdxBarDBCheckLink read GetItem; default;
  end;

  TdxBarDBNavButtonType = (dxbnFirst, dxbnPrior, dxbnNext, dxbnLast,
    dxbnInsert, dxbnDelete, dxbnEdit, dxbnPost, dxbnCancel, dxbnRefresh);
  TdxBarDBNavButtonTypes = set of TdxBarDBNavButtonType;

  TdxBarDBNavButton = class(TdxBarCustomButton)
  private
    FBarDBNavigator: TdxBarDBNavigator;
    FLoadedCaption: string;
    FNavButton: TdxBarDBNavButtonType;
    FIsCaptionAssigned: Boolean;
    FIsHintAssigned: Boolean;
    function DefaultCaption: string;
    procedure ReadCaption(Reader: TReader);
    procedure ReadHint(Reader: TReader);
    procedure SetNavButton(Value: TdxBarDBNavButtonType);
    procedure WriteCaption(Writer: TWriter);
    procedure WriteHint(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function IsCaptionStored: Boolean; override;
    function IsHintStored: Boolean; override;
    procedure Loaded; override;
    procedure SetCaption(const Value: string); override;
    procedure SetHint(Value: string); override;
    procedure TranslationChanged;
  public
    destructor Destroy; override;
    procedure DoClick; override;
  published
  // inherited from TdxBarButton
    property AllowAllUp;
    property ButtonStyle;
    property CloseSubMenuOnClick;
    property DropDownEnabled;
    property GroupIndex;
    property Down;
    property DropDownMenu;
    property Glyph;
    property ImageIndex;
    property LargeGlyph;
    property LargeImageIndex;
    property Lowered;
    property PaintStyle;
    property ShortCut;
    property UnclickAfterDoing;
    property OnClick;

    property BarDBNavigator: TdxBarDBNavigator read FBarDBNavigator write FBarDBNavigator;
    property Caption stored False;
    property Hint stored False;
    property NavButton: TdxBarDBNavButtonType read FNavButton write SetNavButton;
  end;

  TdxBarDBNavigator = class(TComponent, IdxLocalizerListener)
  private
    FBarManager: TdxBarManager;
    FCategoryName: string;
    FConfirmDelete: Boolean;
    FDataLink: TdxBarDBNavDataLink;
    FDBCheckLinks: TdxBarDBCheckLinks;
    FSetVisFlag: Boolean;
    FVisibleButtons: TdxBarDBNavButtonTypes;

    function GetDataSource: TDataSource;
    procedure SetBarManager(Value: TdxBarManager);
    procedure SetCategoryName(Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDBNavItems(Value: TdxBarDBCheckLinks);
    procedure SetVisibleButtons(Value: TdxBarDBNavButtonTypes);

    procedure AddButton(AButton: TdxBarDBNavButton);
    procedure RemoveButton(AButton: TdxBarDBNavButton);
    procedure CheckItemsEnable;
  protected
    Buttons: array[TdxBarDBNavButtonType] of TdxBarDBNavButton;
    procedure ActiveChanged;
    procedure DataChanged;
    procedure EditingChanged;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // IdxLocalizerListener
    procedure TranslationChanged;
  published
    property BarManager: TdxBarManager read FBarManager write SetBarManager;
    property CategoryName: string read FCategoryName write SetCategoryName;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DBCheckLinks: TdxBarDBCheckLinks read FDBCheckLinks write SetDBNavItems;
    property VisibleButtons: TdxBarDBNavButtonTypes read FVisibleButtons
      write SetVisibleButtons default [];
  end;

implementation

{$R dxBarDBNav.res}

uses
  Windows, SysUtils, Forms, dxBarDBNavStrs, Math, StrUtils, Graphics;

const
  BtnResStr = 'DXBARDBNAVBTN_';
  dxBarDBNavBtnName = 'dxBarDBNav';

{ TdxBarDBNavDataLink }

procedure TdxBarDBNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

procedure TdxBarDBNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TdxBarDBNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

{ TdxBarDBCheckLink }

procedure TdxBarDBCheckLink.FreeNotification(Sender: TComponent);
begin
  Free;
end;

function TdxBarDBCheckLink.GetDBNavigator: TdxBarDBNavigator;
begin
  Result := TdxBarDBCheckLinks(Collection).FDBNavigator;
end;

procedure TdxBarDBCheckLink.SetEnableTypes(Value: TdxBarDBEnableTypes);
begin
  if FEnableTypes <> Value then
  begin
    FEnableTypes := Value;
    TdxBarDBCheckLinks(Collection).FDBNavigator.DataChanged;
  end;
end;

procedure TdxBarDBCheckLink.SetItem(Value: TdxBarItem);
begin
  if Value <> FItem then
  begin
    if FItem <> nil then
      FFreeNotificator.RemoveSender(FItem);
    FItem := Value;
    if FItem <> nil then
      FFreeNotificator.AddSender(FItem);
  end;
end;

constructor TdxBarDBCheckLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
end;

destructor TdxBarDBCheckLink.Destroy;
begin
  Item := nil;
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TdxBarDBCheckLink.Assign(Value: TPersistent);
var
  ADBNavItem: TdxBarDBCheckLink;
begin
  if Value is TdxBarDBCheckLink then
  begin
    ADBNavItem := TdxBarDBCheckLink(Value);
    if (ADBNavItem.Item <> nil) and (DBNavigator <> nil) and
      (csUpdating in DBNavigator.ComponentState) and (DBNavigator.BarManager <> nil) then
        Item := DBNavigator.BarManager.GetItemByName(ADBNavItem.Item.Name)
      else
        Item := ADBNavItem.Item;
    FEnableTypes := ADBNavItem.FEnableTypes;
  end
  else
    inherited Assign(Value);
end;

{ TdxBarDBCheckLinks }

function TdxBarDBCheckLinks.GetItem(Index: Integer): TdxBarDBCheckLink;
begin
  if (Index > -1) and (Index < Count) then
    Result := TdxBarDBCheckLink(inherited Items[Index])
  else Result := nil;
end;

function TdxBarDBCheckLinks.GetOwner: TPersistent;
begin
  Result := FDBNavigator;
end;

function TdxBarDBCheckLinks.Add: TdxBarDBCheckLink;
begin
  Result := TdxBarDBCheckLink(inherited Add);
end;

{ dxBarDBNavButton }

destructor TdxBarDBNavButton.Destroy;
begin
  if BarDBNavigator <> nil then
    BarDBNavigator.RemoveButton(Self);
  inherited Destroy;
end;

function TdxBarDBNavButton.DefaultCaption: string;
const
  PResStrs: array[TdxBarDBNavButtonType] of Pointer = (@dxSBAR_BTNCAPTION_FIRST,
    @dxSBAR_BTNCAPTION_PRIOR, @dxSBAR_BTNCAPTION_NEXT, @dxSBAR_BTNCAPTION_LAST,
    @dxSBAR_BTNCAPTION_INSERT, @dxSBAR_BTNCAPTION_DELETE, @dxSBAR_BTNCAPTION_EDIT,
    @dxSBAR_BTNCAPTION_POST, @dxSBAR_BTNCAPTION_CANCEL, @dxSBAR_BTNCAPTION_REFRESH);
begin
  Result := cxGetResourceString(PResStrs[FNavButton]);
end;

procedure TdxBarDBNavButton.ReadCaption(Reader: TReader);
begin
  Caption := Reader.ReadString;
end;

procedure TdxBarDBNavButton.ReadHint(Reader: TReader);
begin
  Hint := Reader.ReadString;
end;

procedure TdxBarDBNavButton.SetNavButton(Value: TdxBarDBNavButtonType);
const
  dxBarNames: array[TdxBarDBNavButtonType] of string =
    ('First', 'Prev', 'Next', 'Last',
     'Insert', 'Delete', 'Edit', 'Post', 'Cancel', 'Refresh');

begin
  if (FNavButton <> Value) or (Name = '') then
  begin
    if csLoading in ComponentState then
      FNavButton := Value
    else
    begin
      if (BarDBNavigator <> nil) and not BarDBNavigator.FSetVisFlag and
        (BarDBNavigator.Buttons[Value] <> nil) then
        raise EdxException.Create(cxGetResourceString(@dxSBAR_DBNAVERROR1));
      if (BarDBNavigator <> nil) and not BarDBNavigator.FSetVisFlag then
        BarDBNavigator.RemoveButton(Self);
      FNavButton := Value;
      if BarDBNavigator <> nil then
      begin
        BarDBNavigator.AddButton(Self);
        Glyph.LoadFromResource(HInstance, PChar(BtnResStr + IntToStr(Integer(FNavButton) + 1)), RT_BITMAP);
      end;
      if BarManager.Designing then
        Name := (BarManager as IdxBarDesigner).UniqueName(dxBarDBNavBtnName + dxBarNames[FNavButton]);
      Caption := DefaultCaption;
      Hint := Caption;
    end;
    RepeatClick := NavButton in [dxbnPrior, dxbnNext];
  end;
end;

procedure TdxBarDBNavButton.WriteCaption(Writer: TWriter);
begin
  Writer.WriteString(Caption);
end;

procedure TdxBarDBNavButton.WriteHint(Writer: TWriter);
begin
  Writer.WriteString(Hint);
end;

procedure TdxBarDBNavButton.DefineProperties(Filer: TFiler);

  function IsCaptionNeedStored: Boolean;
  begin
    Result := ((Filer.Ancestor = nil) or (TdxBarDBNavButton(Filer.Ancestor).Caption <> Caption)) and IsCaptionStored;
  end;

  function IsHintNeedStored: Boolean;
  begin
    Result := ((Filer.Ancestor = nil) or (TdxBarDBNavButton(Filer.Ancestor).Hint <> Hint)) and IsHintStored;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CaptionEx', ReadCaption, WriteCaption, IsCaptionNeedStored);
  Filer.DefineProperty('HintEx', ReadHint, WriteHint, IsHintNeedStored);
end;

function TdxBarDBNavButton.IsCaptionStored: Boolean;
begin
  Result := inherited IsCaptionStored and FIsCaptionAssigned;
end;

function TdxBarDBNavButton.IsHintStored: Boolean;
begin
  Result := inherited IsHintStored and FIsHintAssigned;
end;

procedure TdxBarDBNavButton.Loaded;
begin
  inherited Loaded;
  if BarDBNavigator <> nil then
  begin
    BarDBNavigator.AddButton(Self);
    BarDBNavigator.ActiveChanged;
    Caption := IfThen(FIsCaptionAssigned, FLoadedCaption, DefaultCaption);
  end;
end;

procedure TdxBarDBNavButton.SetCaption(const Value: string);
var
  AHint: string;
  AIsHintAssigned: Boolean;
begin
  if IsLoading then
  begin
    FIsCaptionAssigned := True;
    FLoadedCaption := Value;
  end
  else
  begin
    FIsCaptionAssigned := IsLoading or (Value <> DefaultCaption);
    AHint := Hint;
    AIsHintAssigned := FIsHintAssigned;
    inherited SetCaption(Value);
    if AIsHintAssigned then
      Hint := AHint;
  end;
end;

procedure TdxBarDBNavButton.SetHint(Value: string);
begin
  inherited SetHint(Value);
  FIsHintAssigned := IsLoading or (Value <> Caption);
end;

procedure TdxBarDBNavButton.TranslationChanged;
begin
  if not FIsCaptionAssigned then
    Caption := DefaultCaption;
end;

procedure TdxBarDBNavButton.DoClick;
begin
  inherited;
  if Assigned(OnClick) then Exit;
  if BarDBNavigator <> nil then
    with BarDBNavigator.FDataLink.DataSet do
      case FNavButton of
        dxbnFirst: First;
        dxbnPrior: Prior;
        dxbnNext: Next;
        dxbnLast: Last;
        dxbnInsert: Insert;
        dxbnDelete:
          if not BarDBNavigator.ConfirmDelete or
            (Application.MessageBox(PChar(cxGetResourceString(@dxSBAR_DELETERECORD)),
               PChar(Application.Title), MB_ICONQUESTION or MB_YESNO) = ID_YES) then
            Delete;
        dxbnEdit: Edit;
        dxbnPost: Post;
        dxbnCancel: Cancel;
        dxbnRefresh: Refresh;
      end;
end;

{ TdxBarDBNavigator }

constructor TdxBarDBNavigator.Create(AOwner: TComponent);
var
  ABarManager: TdxBarManager;
begin
  ABarManager := GetBarManagerForComponent(AOwner);
  {if (ABarManager = nil) and (dxBarManagerList.Count <> 0) then
    ABarManager := dxBarManagerList[0];}
  inherited Create(AOwner);
  FBarManager := ABarManager;
  FDataLink := TdxBarDBNavDataLink.Create;
  FDataLink.FNavigator := Self;
  FCategoryName := cxGetResourceString(@dxSBAR_DBNAVIGATORCATEGORYNAME);
  FDBCheckLinks := TdxBarDBCheckLinks.Create(TdxBarDBCheckLink);
  FDBCheckLinks.FDBNavigator := Self;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TdxBarDBNavigator.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  VisibleButtons := [];
  FDBCheckLinks.Free;
  FDataLink.FNavigator := nil;
  FDataLink.Free;
  inherited Destroy;
end;

procedure TdxBarDBNavigator.TranslationChanged;
var
  I: TdxBarDBNavButtonType;
begin
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I] <> nil then
      Buttons[I].TranslationChanged;
end;

procedure TdxBarDBNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = BarManager then BarManager := nil;
    if AComponent = DataSource then DataSource := nil;
  end;
end;

procedure TdxBarDBNavigator.CheckItemsEnable;
var
  I: Integer;
  Flag: Boolean;
begin
  for I := 0 to FDBCheckLinks.Count - 1 do
    with FDBCheckLinks[I] do
      if Item <> nil then
      begin
        Flag := FDataLink.Active;
        if dxdbtCanModify in EnableTypes then
          Flag := Flag and FDataLink.DataSet.CanModify;
        if dxdbtNotEOF in EnableTypes then
          Flag := Flag and not FDataLink.DataSet.EOF;
        if dxdbtNotBOF in EnableTypes then
          Flag := Flag and not FDataLink.DataSet.BOF;
        if dxdbtHasRecords in EnableTypes then
          Flag := Flag and not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
        if dxdbtIsModified in EnableTypes then
          Flag := Flag and FDataLink.Editing;
        if dxdbtIsNotModified in EnableTypes then
          Flag := Flag and not FDataLink.Editing;
        Item.Enabled := Flag;
      end;
end;

procedure TdxBarDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := FDataLink.Active and not FDataLink.DataSet.EOF;
  if Buttons[dxbnFirst] <> nil then
    Buttons[dxbnFirst].Enabled := UpEnable;
  if Buttons[dxbnPrior] <> nil then
    Buttons[dxbnPrior].Enabled := UpEnable;
  if Buttons[dxbnNext] <> nil then
    Buttons[dxbnNext].Enabled := DnEnable;
  if Buttons[dxbnLast] <> nil then
    Buttons[dxbnLast].Enabled := DnEnable;
  if Buttons[dxbnDelete] <> nil then
    Buttons[dxbnDelete].Enabled := FDataLink.Active and FDataLink.DataSet.CanModify and
      not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  if Buttons[dxbnRefresh] <> nil then
    Buttons[dxbnRefresh].Enabled := FDataLink.Active;
  CheckItemsEnable;
end;

procedure TdxBarDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := FDataLink.Active and FDataLink.DataSet.CanModify;
  if Buttons[dxbnInsert] <> nil then
    Buttons[dxbnInsert].Enabled := CanModify;
  if Buttons[dxbnEdit] <> nil then
    Buttons[dxbnEdit].Enabled := CanModify and not FDataLink.Editing;
  if Buttons[dxbnPost] <> nil then
    Buttons[dxbnPost].Enabled := CanModify and FDataLink.Editing;
  if Buttons[dxbnCancel] <> nil then
    Buttons[dxbnCancel].Enabled := CanModify and FDataLink.Editing;
  CheckItemsEnable;
end;

procedure TdxBarDBNavigator.ActiveChanged;
var
  I: TdxBarDBNavButtonType;
  J: Integer;
begin
  if not FDataLink.Active then
  begin
    for I := Low(Buttons) to High(Buttons) do
      if Buttons[I] <> nil then
        Buttons[I].Enabled := False;
    for J := 0 to FDBCheckLinks.Count - 1 do
      if FDBCheckLinks[J].Item <> nil then
        FDBCheckLinks[J].Item.Enabled := False;
  end
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

function TdxBarDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdxBarDBNavigator.SetBarManager(Value: TdxBarManager);
var
  SaveVisButtons: TdxBarDBNavButtonTypes;
begin
  if FBarManager <> Value then
  begin
    SaveVisButtons := FVisibleButtons;
    VisibleButtons := [];
    FBarManager := Value;
    VisibleButtons := SaveVisButtons;
  end;
end;

procedure TdxBarDBNavigator.SetCategoryName(Value: string);
var
  Index1, Index2: Integer;
  I: TdxBarDBNavButtonType;
begin
  if FCategoryName <> Value then
    if csLoading in ComponentState then
      FCategoryName := Value
    else
      if BarManager <> nil then
      begin
        Index1 := BarManager.Categories.IndexOf(FCategoryName);
        FCategoryName := Value;
        Index2 := BarManager.Categories.IndexOf(FCategoryName);
        if Index2 = -1 then
        begin
          BarManager.Categories.Add(FCategoryName);
          Index2 := BarManager.Categories.IndexOf(FCategoryName);
        end;
        for I := Low(Buttons) to High(Buttons) do
          if Buttons[I] <> nil then Buttons[I].Category := Index2;
        if (Index1 > -1) and (BarManager.GetCountByCategory(Index1) = 0) then
          BarManager.Categories.Delete(Index1);
      end;
end;

procedure TdxBarDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then ActiveChanged;
  cxAddFreeNotification(Self, Value);
end;

procedure TdxBarDBNavigator.SetDBNavItems(Value: TdxBarDBCheckLinks);
begin
  FDBCheckLinks.Assign(Value);
  DataChanged;
end;

procedure TdxBarDBNavigator.SetVisibleButtons(Value: TdxBarDBNavButtonTypes);
var
  I: TdxBarDBNavButtonType;
  AIndex: Integer;
begin
  if FVisibleButtons <> Value then
  begin
    FVisibleButtons := Value;
    FSetVisFlag := True;
    if not (csLoading in ComponentState) and (BarManager <> nil) then
      for I := Low(Buttons) to High(Buttons) do
      begin
        if (Buttons[I] <> nil) and //not (csDestroying in Buttons[I].ComponentState) and
          not (I in Value) then
        begin
          Buttons[I].Free;
          Buttons[I] := nil;
        end;
        if (Buttons[I] = nil) and (I in Value) then
        begin
          Buttons[I] := TdxBarDBNavButton.Create(Owner);
          with Buttons[I] do
          begin
            BarDBNavigator := Self;
            AIndex := BarManager.Categories.IndexOf(FCategoryName);
            if AIndex = -1 then
            begin
              BarManager.Categories.Add(FCategoryName);
              AIndex := BarManager.Categories.IndexOf(FCategoryName);
            end;
            Category := AIndex;
            NavButton := I;
          end;
        end;
      end;
    ActiveChanged;
    FSetVisFlag := False;
  end;
  if (FVisibleButtons = []) and (BarManager <> nil) and
    not (csDestroying in BarManager.ComponentState) then
  begin
    AIndex := BarManager.Categories.IndexOf(FCategoryName);
    if (AIndex > -1) and (BarManager.GetCountByCategory(AIndex) = 0) then
      BarManager.Categories.Delete(AIndex);
  end;
end;

procedure TdxBarDBNavigator.AddButton(AButton: TdxBarDBNavButton);
begin
  Buttons[AButton.NavButton] := AButton;
  FVisibleButtons := FVisibleButtons + [AButton.NavButton];
end;

procedure TdxBarDBNavigator.RemoveButton(AButton: TdxBarDBNavButton);
begin
  Buttons[AButton.NavButton] := nil;
  FVisibleButtons := FVisibleButtons - [AButton.NavButton];
end;

initialization
  dxBarRegisterItem(TdxBarDBNavButton, TdxBarButtonControl, False);
  GroupDescendentsWith(TdxBarDBNavigator, TControl);

finalization
  dxBarUnregisterItem(TdxBarDBNavButton);

end.
