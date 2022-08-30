unit uCloudStorageDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Generics.Collections, dxCore,
  Controls, Forms, BaseForm, Menus, StdCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, dxLayoutContainer,
  cxListView, cxContainer, cxEdit, cxTreeView, cxClasses, dxLayoutControl,
  dxCloudStorage, dxAuthorizationAgents, ImgList, cxImageList,
  dxActivityIndicator, dxAlertWindow, dxLayoutControlAdapters, cxButtons,
  ActnList, Dialogs;

type

  TfmCloudStorageDemoForm = class(TfmBaseForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    tvMain: TcxTreeView;
    dxLayoutItem1: TdxLayoutItem;
    lvMain: TcxListView;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    aaGDrive: TdxGoogleAPIOAuth2AuthorizationAgent;
    aaOneDrive: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent;
    csMain: TdxCloudStorage;
    Options1: TMenuItem;
    miGoogleDrive: TMenuItem;
    miMicrosoftOneDrive: TMenuItem;
    miProviders: TMenuItem;
    SpecifyAuthorizationSettings1: TMenuItem;
    il16x16: TcxImageList;
    il32x32: TcxImageList;
    ilSystem: TImageList;
    aiMain: TdxActivityIndicator;
    awmMain: TdxAlertWindowManager;
    lgTools: TdxLayoutGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    cxButton1: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    cxButton2: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    cxButton3: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    alMain: TActionList;
    ilActions: TcxImageList;
    acCreateFolder: TAction;
    acUploadFile: TAction;
    acDelete: TAction;
    acRefresh: TAction;
    cxButton4: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    FileOpenDialog1: TFileOpenDialog;
    procedure SpecifyAuthorizationSettings1Click(Sender: TObject);
    procedure miChooseProviderClick(Sender: TObject);
    procedure tvMainEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure lvMainEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure csMainConnectedChanged(Sender: TObject);
    procedure csMainTreeDataLoaded(Sender: TObject;
      AFolder: TdxCloudStorageCustomFolder);
    procedure tvMainExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvMainClick(Sender: TObject);
    procedure tvMainKeyPress(Sender: TObject; var Key: Char);
    procedure lvMainGetImageIndex(Sender: TObject; Item: TListItem);
    procedure lvMainDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure csMainItemDownloaded(Sender: TObject;
      const AItem: TdxCloudStorageItem; AStream: TStream);
    procedure csMainItemDownloading(Sender: TObject;
      const AItem: TdxCloudStorageItem; const ASize: Integer);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acCreateFolderExecute(Sender: TObject);
    procedure acUploadFileExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure csMainFolderCreated(Sender: TObject;
      AFolder: TdxCloudStorageCustomFolder);
    procedure csMainItemMovedToTrash(Sender: TObject;
      const AItem: TdxCloudStorageItem);
    procedure csMainItemUploading(Sender: TObject; const AFileName: string;
      const ASize: Integer);
    procedure csMainError(Sender: TObject; const AErrorObject);
  private
    FSelectedNode: TTreeNode;
    FIconsMap: TDictionary<Integer, Integer>;
    procedure DoChooseProvider(ATag: Integer);
    function GetFileName(AFile: TdxCloudStorageItem): string;
    function SizeToString(ASize: Integer): string;
    function GetTopFolder: TdxCloudStorageCustomFolder;
    procedure PopulateListItems;
    procedure PopulateNodes(AParentNode: TTreeNode);
    procedure SetSelectedNode(const Value: TTreeNode);
    procedure ShowSetup(AIsFirstTime: Boolean = False);
    procedure WaitForFolderLoaded(AFolder: TdxCloudStorageCustomFolder);
  protected
    property SelectedNode: TTreeNode read FSelectedNode write SetSelectedNode;
  end;

var
  fmCloudStorageDemoForm: TfmCloudStorageDemoForm;

implementation

uses
  ShellApi, IOUtils,
  uCloudSetupForm, dxCloudStorageMicrosoftOneDriveProvider,
  dxCloudStorageGoogleDriveProvider, DemoUtils, dxWinInet;

{$R *.dfm}

var
  FImageListHandle: Cardinal;
  FInfo: TSHFileInfo;

function ItemsCompare(AItem1, AItem2: TdxCloudStorageItem): Integer;
begin
  if AItem1.IsFolder xor AItem2.IsFolder then
  begin
    if AItem1.IsFolder then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := CompareText(AItem1.Name, AItem2.Name);
end;

function NodesCompare(lParam1, lParam2, lParamSort: TdxNativeInt): Integer stdcall;
begin
  Result := ItemsCompare(TdxCloudStorageItem(TTreeNode(lParam1).Data), TdxCloudStorageItem(TTreeNode(lParam2).Data));
end;

function ListItemsCompare(lParam1, lParam2, lParamSort: TdxNativeInt): Integer stdcall;
begin
  Result := ItemsCompare(TdxCloudStorageItem(TListItem(lParam1).Data), TdxCloudStorageItem(TListItem(lParam2).Data));
end;

procedure TfmCloudStorageDemoForm.miChooseProviderClick(Sender: TObject);
begin
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miGoogleDrive'), False);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miMicrosoftOneDrive'), False);
  DoChooseProvider(TComponent(Sender).Tag);
end;

procedure TfmCloudStorageDemoForm.lvMainDblClick(Sender: TObject);
var
  AItem: TdxCloudStorageItem;
  I: Integer;
begin
  if lvMain.Selected = nil then
    Exit;
  AItem := TdxCloudStorageItem(lvMain.Selected.Data);
  if AItem.IsFolder then
  begin
    if tvMain.Selected <> nil then
    begin
      if tvMain.Selected.Data = AItem then
        SelectedNode := tvMain.Selected
      else
      begin
        for I := 0 to tvMain.Selected.Count - 1 do
          if tvMain.Selected.Item[I].Data = AItem then
          begin
            tvMain.Selected := tvMain.Selected.Item[I];
            SelectedNode := tvMain.Selected;
            Break;
          end;
      end;
    end;
    if tvMain.Selected.Data <> AItem then
      for I := 0 to tvMain.Items.Count - 1 do
        if tvMain.Items[I].Data = AItem then
        begin
          tvMain.Selected := tvMain.Items[I];
          SelectedNode := tvMain.Selected;
          Break;
        end;
  end
  else
    TdxCloudStorageFile(AItem).DownloadContent;
end;

procedure TfmCloudStorageDemoForm.lvMainEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmCloudStorageDemoForm.lvMainGetImageIndex(Sender: TObject;
  Item: TListItem);
var
  AItem: TdxCloudStorageItem;
  AFlags: Integer;
  AInfo: TSHFileInfo;
  AIndex: Integer;
  AResult: Integer;
  AImage: TBitmap;
begin
  AItem := TdxCloudStorageItem(Item.Data);
  if AItem.IsFolder then
    Exit;
  FillChar(AInfo, SizeOf(AInfo), 0);
  AFlags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PChar(GetFileName(AItem)), 0, AInfo, SizeOf(AInfo), AFlags);
  AIndex := AInfo.iIcon;
  DestroyIcon(AInfo.hIcon);

  if not FIconsMap.TryGetValue(AIndex, AResult) then
  begin
    AResult := il32x32.Count;
    FIconsMap.Add(AIndex, AResult);
    AImage := TBitmap.Create;
    try
      ilSystem.GetBitmap(AIndex, AImage);
      il32x32.Add(AImage, nil);
    finally
      AImage.Free;
    end;
  end;
  Item.ImageIndex := AResult;
end;

procedure TfmCloudStorageDemoForm.lvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    lvMainDblClick(Sender);
end;

procedure TfmCloudStorageDemoForm.tvMainClick(Sender: TObject);
begin
  SelectedNode := tvMain.Selected;
end;

procedure TfmCloudStorageDemoForm.tvMainEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmCloudStorageDemoForm.tvMainExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  AFolder: TdxCloudStorageCustomFolder;
begin
  AFolder := TdxCloudStorageCustomFolder(Node.Data);
  if not AFolder.IsLoaded then
  begin
    AFolder.FetchChildren(True);
    WaitForFolderLoaded(AFolder);
    AllowExpansion := AFolder.HasChildren
  end;
end;

procedure TfmCloudStorageDemoForm.tvMainKeyPress(Sender: TObject;
  var Key: Char);
begin
  SelectedNode := tvMain.Selected;
end;

procedure TfmCloudStorageDemoForm.acCreateFolderExecute(Sender: TObject);
var
  AName: string;
begin
  AName := 'New Folder';
  if InputQuery('Create Folder', 'Name', AName) then
    TdxCloudStorageFolder(tvMain.Selected.Data).CreateFolder(AName);
end;

procedure TfmCloudStorageDemoForm.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure want to delete the item(s)?', mtConfirmation, mbYesNoCancel, 0) = mrYes then
    TdxCloudStorageItem(lvMain.Selected.Data).MoveToTrash;
end;

procedure TfmCloudStorageDemoForm.acRefreshExecute(Sender: TObject);
begin
  TdxCloudStorageCustomFolder(SelectedNode.Data).FetchChildren(True);
end;

procedure TfmCloudStorageDemoForm.acUploadFileExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    TdxCloudStorageFolder(SelectedNode.Data).UploadFile(FileOpenDialog1.FileName);
end;

procedure TfmCloudStorageDemoForm.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  ATopFolder: TdxCloudStorageCustomFolder;
begin
  ATopFolder := GetTopFolder;
  acCreateFolder.Enabled := (ATopFolder <> nil) and ATopFolder.IsRoot;
  acUploadFile.Enabled := (ATopFolder <> nil) and ATopFolder.IsRoot;
  acDelete.Enabled := (ATopFolder <> nil) and ATopFolder.IsRoot and (lvMain.Selected <> nil);
  acRefresh.Enabled := ATopFolder <> nil;
end;

procedure TfmCloudStorageDemoForm.csMainConnectedChanged(Sender: TObject);
const
  AImageIndexMap: array[TdxCloudStorageSpecialFolder.TType] of Integer = (4, 2, 2, 3, 0, 5);
var
  ANode: TTreeNode;
begin
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miGoogleDrive'), csMain.ProviderClass = TdxCloudStorageGoogleDriveProvider);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miMicrosoftOneDrive'), csMain.ProviderClass = TdxCloudStorageMicrosoftOneDriveProvider);

  tvMain.Items.BeginUpdate;
  try
    tvMain.Items.Clear;
    lvMain.Items.Clear;
    if csMain.Connected then
    begin
      ANode := tvMain.Items.AddObject(nil, 'Root', csMain.Files.Root);
      ANode.ImageIndex := 1;
      ANode.SelectedIndex := 1;
      with lvMain.Items.Add do
      begin
        ImageIndex := 1;
        Caption := 'Root';
        Data := csMain.Files.Root;
      end;
      PopulateNodes(ANode);
      csMain.Files.Root.FetchChildren;
      csMain.Files.SpecialFolders.Enum( procedure (const ASpecialFolder: TdxCloudStorageSpecialFolder)
        begin
          ANode := tvMain.Items.AddObject(nil, ASpecialFolder.Name, ASpecialFolder);
          ANode.ImageIndex := AImageIndexMap[ASpecialFolder.&Type];
          ANode.SelectedIndex := ANode.ImageIndex;
          with lvMain.Items.Add do
          begin
            ImageIndex := ANode.ImageIndex;
            Caption := ANode.Text;
            Data := ANode.Data;
          end;
          PopulateNodes(ANode);
        end);
      tvMain.Selected := nil;
    end;
  finally
    tvMain.Items.EndUpdate;
  end;
end;

procedure TfmCloudStorageDemoForm.csMainError(Sender: TObject;
  const AErrorObject);
var
  AError: TdxJSONObject;
begin
  inherited;
  AError := TdxJSONObject(AErrorObject);
  MessageDlg(AError.GetChildParamValue('error', 'message'), mtError, [mbOK], 0);
end;

procedure TfmCloudStorageDemoForm.csMainFolderCreated(Sender: TObject;
  AFolder: TdxCloudStorageCustomFolder);
var
  I: Integer;
begin
  TdxCloudStorageCustomFolder(SelectedNode.Data).FetchChildren(True);
  for I := 0 to AFolder.Parents.Count - 1 do
    AFolder.Parents[I].FetchChildren;
  PopulateListItems;
  for I := 0 to lvMain.Items.Count - 1 do
    if lvMain.Items[I].Data = AFolder then
    begin
      lvMain.Selected := lvMain.Items[I];
      Break;
    end;
end;

procedure TfmCloudStorageDemoForm.csMainTreeDataLoaded(Sender: TObject;
  AFolder: TdxCloudStorageCustomFolder);

  procedure ForEachNode(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode.Data = AFolder then
      PopulateNodes(ANode);
    for I := 0 to ANode.Count - 1 do
      ForEachNode(ANode.Item[I]);
  end;

var
  ANode: TTreeNode;
  I: Integer;
begin
  if AFolder = nil then
    Exit;
  ANode := tvMain.InnerTreeView.TopItem;
  while ANode <> nil do
  begin
    if ANode.Parent = nil then
      ForEachNode(ANode);
    ANode := ANode.GetNext;
  end;
  if (SelectedNode <> nil) and (SelectedNode.Data = AFolder) then
    PopulateListItems
  else
  for I := 0 to lvMain.Items.Count - 1 do
    if lvMain.Items[I].Data = AFolder then
    begin
      lvMain.Items[I].Caption := AFolder.Name;
      Break;
    end;

end;

procedure TfmCloudStorageDemoForm.csMainItemDownloaded(Sender: TObject;
  const AItem: TdxCloudStorageItem; AStream: TStream);
var
  AFileStream: TFileStream;
  ADir: string;
  AFileName: string;
  ABaseName: string;
  ABaseFileName: string;
  ABaseExt: string;
  I: Integer;
begin
  ADir := TPath.GetTempPath;
  ABaseName := GetFileName(AItem);
  ABaseFileName := ExtractFileName(ABaseName);
  ABaseExt := ExtractFileExt(ABaseName);
  AFileName := ADir + ABaseName;
  I := 1;
  while FileExists(AFileName) do
  begin
    AFileName := ADir + Format('%s[%d].%s', [ABaseFileName, I, ABaseExt]);
    Inc(I);
  end;

  AFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    AFileStream.CopyFrom(AStream, 0);
  finally
    AFileStream.Free;
  end;
  ShellExecute(0, 'open', PWideChar(AFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TfmCloudStorageDemoForm.csMainItemDownloading(Sender: TObject;
  const AItem: TdxCloudStorageItem; const ASize: Integer);
begin
  inherited;
  if ASize = -1 then
    awmMain.Show('Downloading...', Format('File: %s', [AItem.Name]))
  else
    awmMain.Show('Downloading...', Format('File: %s'#13#10'Size: %s', [AItem.Name, SizeToString(ASize)]));
end;

procedure TfmCloudStorageDemoForm.csMainItemMovedToTrash(Sender: TObject;
  const AItem: TdxCloudStorageItem);
begin
  if csMain.Files.Trash <> nil then
    csMain.Files.Trash.FetchChildren;
  if SelectedNode <> nil then
    TdxcloudStorageCustomFolder(SelectedNode.Data).FetchChildren(True);
  PopulateListItems;
end;

procedure TfmCloudStorageDemoForm.csMainItemUploading(Sender: TObject;
  const AFileName: string; const ASize: Integer);
begin
  if ASize = -1 then
    awmMain.Show('Uploading...', Format('File: %s', [AFileName]))
  else
    awmMain.Show('Uploading...', Format('File: %s'#13#10'Size: %s', [AFileName, SizeToString(ASize)]));
end;

function TfmCloudStorageDemoForm.GetFileName(AFile: TdxCloudStorageItem): string;
var
  AExt: string;
begin
  Result := AFile.Name;
  AExt := csMain.Provider.GetExtension(AFile);
  if (AExt <>  '') and (Pos(LowerCase(AExt), LowerCase(Result)) = 0) then
    Result := Format('%s%s', [Result, AExt]);
end;

function TfmCloudStorageDemoForm.SizeToString(ASize: Integer): string;
begin
  if ASize > 1024 * 1024 then
    Result := Format('%3.2f MB', [ASize / 1024 / 1024])
  else if ASize > 1024 then
    Result := Format('%3.2f KB', [ASize / 1024])
  else
    Result := Format('%d B', [ASize])


end;

function TfmCloudStorageDemoForm.GetTopFolder: TdxCloudStorageCustomFolder;

  function GetParent(ANode: TTreeNode): TTreeNode;
  begin
    if ANode.Parent = nil then
      Result := ANode
    else
      Result := GetParent(ANode.Parent);
  end;

begin
  Result := nil;
  if SelectedNode = nil then
    Exit;
  Result := TdxCloudStorageCustomFolder(GetParent(tvMain.Selected).Data);
end;

procedure TfmCloudStorageDemoForm.PopulateListItems;
var
  I: Integer;
  AFolder: TdxCloudStorageCustomFolder;
begin
  if SelectedNode = nil then
    Exit;
  AFolder := TdxCloudStorageCustomFolder(SelectedNode.Data);
  if not AFolder.IsLoaded then
  begin
    AFolder.FetchChildren;
    WaitForFolderLoaded(AFolder);
  end;
  lvMain.Items.BeginUpdate;
  try
    lvMain.Clear;
    for I := 0 to AFolder.Children.Count - 1 do
      with lvMain.Items.Add do
      begin
        Caption := AFolder.Children[I].Name;
        Data := AFolder.Children[I];
      end;
    lvMain.CustomSort(ListItemsCompare, 0);
  finally
    lvMain.Items.EndUpdate;
  end;
end;

procedure TfmCloudStorageDemoForm.PopulateNodes(AParentNode: TTreeNode);
var
  AFolder: TdxCloudStorageCustomFolder;
  I: Integer;
begin
  AFolder := TdxCloudStorageCustomFolder(AParentNode.Data);
  AParentNode.Text := AFolder.Name;
  if not AFolder.IsLoaded then
  begin
    if AParentNode.Count = 0 then
      tvMain.Items.AddChild(AParentNode, '(loading...)')
  end
  else
  begin
    tvMain.Items.BeginUpdate;
    try
      AParentNode.DeleteChildren;
      for I := 0 to AFolder.Children.Count - 1 do
        if AFolder.Children[I] is TdxCloudStorageCustomFolder then
          PopulateNodes(tvMain.Items.AddChildObject(AParentNode, AFolder.Children[I].Name, AFolder.Children[I]));
      AParentNode.CustomSort(NodesCompare, 0, False);
    finally
      tvMain.Items.EndUpdate;
    end;
  end;
end;

procedure TfmCloudStorageDemoForm.SetSelectedNode(const Value: TTreeNode);
begin
  if FSelectedNode = Value then
    Exit;
  FSelectedNode := Value;
  PopulateListItems;
end;

procedure TfmCloudStorageDemoForm.DoChooseProvider(ATag: Integer);
begin
  if ATag = 0 then
  begin
    if aaOneDrive.ClientID <> '' then
      csMain.ProviderClass := TdxCloudStorageMicrosoftOneDriveProvider
    else
      if aaGDrive.ClientID <> '' then
        csMain.ProviderClass := TdxCloudStorageGoogleDriveProvider
      else
        csMain.ProviderClass := nil;
  end
  else
  begin
    if aaGDrive.ClientID <> '' then
      csMain.ProviderClass := TdxCloudStorageGoogleDriveProvider
    else
      if aaOneDrive.ClientID <> '' then
        csMain.ProviderClass := TdxCloudStorageMicrosoftOneDriveProvider
      else
        csMain.ProviderClass := nil;
  end;
  if csMain.ProviderClass = TdxCloudStorageGoogleDriveProvider then
    csMain.Provider.AuthorizationAgent := aaGDrive
  else
    if csMain.ProviderClass = TdxCloudStorageMicrosoftOneDriveProvider then
      csMain.Provider.AuthorizationAgent := aaOneDrive;
  csMain.Connected := True;
end;

procedure TfmCloudStorageDemoForm.FormCreate(Sender: TObject);
begin
  inherited;
  FImageListHandle := SHGetFileInfo('C:\', 0, FInfo, SizeOf(FInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  ilSystem.Handle := FImageListHandle;
  FIconsMap := TDictionary<Integer, Integer>.Create;
end;

procedure TfmCloudStorageDemoForm.FormDestroy(Sender: TObject);
begin
  ilSystem.Handle := 0;
  DestroyIcon(FInfo.hIcon);
  FreeResource(FImageListHandle);
  FIconsMap.Free;
  inherited;
end;

procedure TfmCloudStorageDemoForm.FormShow(Sender: TObject);
begin
  inherited;
  ShowSetup(True);
end;

procedure TfmCloudStorageDemoForm.ShowSetup(AIsFirstTime: Boolean = False);
begin
  with TfmCloudSetupWizard.Create(nil) do
  try
    teGoogleApiClientID.Text := aaGDrive.ClientID;
    teGoogleApiClientSecret.Text := aaGDrive.ClientSecret;
    teMSGraphClientID.Text := aaOneDrive.ClientID;
    teMSGraphClientSecret.Text := aaOneDrive.ClientSecret;
    if not AIsFirstTime then
      btnStart.Caption := 'OK';
    if ShowModal = mrOk then
    begin
      aaGDrive.ClientID := teGoogleApiClientID.Text;
      aaGDrive.ClientSecret := teGoogleApiClientSecret.Text;
      aaOneDrive.ClientID := teMSGraphClientID.Text;
      aaOneDrive.ClientSecret := teMSGraphClientSecret.Text;
      dxDemoMenuItemSetEnabled(dxDemoFindMenuItem(mmMain, 'miGoogleDrive'), (aaGDrive.ClientID <> '') and (aaGDrive.ClientSecret <> ''));
      dxDemoMenuItemSetEnabled(dxDemoFindMenuItem(mmMain, 'miMicrosoftOneDrive'), (aaOneDrive.ClientID <> '') and (aaOneDrive.ClientSecret <> ''));
      DoChooseProvider(dxLayoutGroup4.ItemIndex);
    end;
  finally
    Free;
  end;
end;

procedure TfmCloudStorageDemoForm.WaitForFolderLoaded(AFolder: TdxCloudStorageCustomFolder);
begin
  if AFolder.IsLoaded then
    Exit;
  if not aiMain.Active then
  begin
    aiMain.Active := True;
    aiMain.Left := lvMain.Left + (lvMain.Width - aiMain.Width) div 2;
    aiMain.Top := lvMain.Top + (lvMain.Height - aiMain.Height) div 2;
    aiMain.Visible := True;
  end;
  while not AFolder.IsLoaded do
    Application.ProcessMessages;

  aiMain.Active := False;
  aiMain.Visible := False;
end;

procedure TfmCloudStorageDemoForm.SpecifyAuthorizationSettings1Click(
  Sender: TObject);
begin
  ShowSetup;
end;

end.
