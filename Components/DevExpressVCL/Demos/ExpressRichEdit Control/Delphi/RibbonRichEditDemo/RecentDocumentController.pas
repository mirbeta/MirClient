unit RecentDocumentController;

interface

{$I cxVer.Inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, IniFiles, dxRibbonBackstageViewGalleryControl, dxRibbonBackstageView;

type
{ TdxRibbonRecentDocumentsController }

  TdxRibbonRecentDocumentsController = class
  private
    FMaxCount: Integer;
    FRecentDocumentsGroup: TdxRibbonBackstageViewGalleryGroup;
    FRecentPathsGroup: TdxRibbonBackstageViewGalleryGroup;

    procedure DeleteExcessItems(AItems: TdxRibbonBackstageViewGalleryItems);
    function GetItemByValue(AItems: TdxRibbonBackstageViewGalleryItems; const AValue: string): TdxRibbonBackstageViewGalleryItem;
    function InternalAdd(AItems: TdxRibbonBackstageViewGalleryItems; const AValue: string; AImageIndex: Integer): TdxRibbonBackstageViewGalleryItem;
    procedure InternalLoad(AItems: TdxRibbonBackstageViewGalleryItems; AIniFile: TCustomIniFile;
      const ASection: string; AImageIndex: Integer);
    procedure InternalSave(AItems: TdxRibbonBackstageViewGalleryItems; AIniFile: TCustomIniFile; const ASection: string);
  protected
    procedure DoLoad(AConfig: TCustomIniFile); virtual;
    procedure DoSave(AConfig: TCustomIniFile); virtual;
  public
    constructor Create(ARecentDocuments, ARecentPaths: TdxRibbonBackstageViewGalleryGroups);
    procedure Add(const AFileName: string); virtual;
    //
    procedure LoadFromIniFile(const AFileName: string);
    procedure SaveToIniFile(const AFileName: string);

    property MaxCount: Integer read FMaxCount write FMaxCount;
  end;

implementation

uses
  dxGalleryControl;

{ TdxRibbonRecentDocumentsController }

constructor TdxRibbonRecentDocumentsController.Create(ARecentDocuments, ARecentPaths: TdxRibbonBackstageViewGalleryGroups);
begin
  inherited Create;
  FRecentDocumentsGroup := ARecentDocuments.Add;
  FRecentPathsGroup := ARecentPaths.Add;
end;

procedure TdxRibbonRecentDocumentsController.Add(const AFileName: string);
var
  AItems: TdxRibbonBackstageViewGalleryItems;
  AImageIndex: Integer;
begin
  AItems := FRecentDocumentsGroup.Items;
  AImageIndex := FRecentDocumentsGroup.GetParentComponent.Tag;
  InternalAdd(AItems, AFileName, AImageIndex);
  AItems := FRecentPathsGroup.Items;
  AImageIndex := FRecentPathsGroup.GetParentComponent.Tag;
  InternalAdd(AItems, ExtractFileDir(AFileName), AImageIndex);
end;

procedure TdxRibbonRecentDocumentsController.LoadFromIniFile(
  const AFileName: string);
var
  AIniFile: TIniFile;
begin
  AIniFile := TIniFile.Create(AFileName);
  try
    DoLoad(AIniFile);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxRibbonRecentDocumentsController.SaveToIniFile(
  const AFileName: string);
var
  AIniFile: TIniFile;
begin
  AIniFile := TIniFile.Create(AFileName);
  try
    DoSave(AIniFile);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxRibbonRecentDocumentsController.DoLoad(AConfig: TCustomIniFile);
var
  AItems: TdxRibbonBackstageViewGalleryItems;
  AImageIndex: Integer;
begin
  AItems := FRecentDocumentsGroup.Items;
  AImageIndex := FRecentDocumentsGroup.GetParentComponent.Tag;
  InternalLoad(AItems, AConfig, 'RecentDocuments', AImageIndex);
  AItems := FRecentPathsGroup.Items;
  AImageIndex := FRecentPathsGroup.GetParentComponent.Tag;
  InternalLoad(AItems, AConfig, 'RecentPaths', AImageIndex);
end;

procedure TdxRibbonRecentDocumentsController.DoSave(AConfig: TCustomIniFile);
begin
  InternalSave(FRecentDocumentsGroup.Items, AConfig, 'RecentDocuments');
  InternalSave(FRecentPathsGroup.Items, AConfig, 'RecentPaths');
end;

procedure TdxRibbonRecentDocumentsController.DeleteExcessItems(AItems: TdxRibbonBackstageViewGalleryItems);
var
  I: Integer;
begin
  for I := AItems.Count - 1 downto MaxCount do
    AItems.Delete(I);
end;

function TdxRibbonRecentDocumentsController.GetItemByValue(
  AItems: TdxRibbonBackstageViewGalleryItems; const AValue: string): TdxRibbonBackstageViewGalleryItem;
var
  I: Integer;
  AItem: TdxRibbonBackstageViewGalleryItem;
begin
  Result := nil;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[I];
    if SameText(AItem.Hint, AValue) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxRibbonRecentDocumentsController.InternalAdd(
  AItems: TdxRibbonBackstageViewGalleryItems; const AValue: string; AImageIndex: Integer): TdxRibbonBackstageViewGalleryItem;
begin
  Result := GetItemByValue(AItems, AValue);
  if Result = nil then
  begin
    Result := AItems.Add;
    Result.Caption := ExtractFileName(AValue);
    Result.Hint := AValue;
    Result.Description := StringReplace(AValue, '\', ' '#187' ', [rfReplaceAll]);
    Result.ImageIndex := AImageIndex;
  end;
  Result.Index := 0;
  DeleteExcessItems(AItems);
end;

procedure TdxRibbonRecentDocumentsController.InternalLoad(AItems: TdxRibbonBackstageViewGalleryItems;
  AIniFile: TCustomIniFile; const ASection: string; AImageIndex: Integer);
var
  AItem: TdxRibbonBackstageViewGalleryItem;
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to AIniFile.ReadInteger(ASection, 'Count', 0) - 1 do
    begin
      AItem := InternalAdd(AItems, AIniFile.ReadString(ASection, IntToStr(I), ''), AImageIndex);
      AItem.Pinned := AIniFile.ReadBool(ASection, IntToStr(I) + 'Pinned', False);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

procedure TdxRibbonRecentDocumentsController.InternalSave(
  AItems: TdxRibbonBackstageViewGalleryItems; AIniFile: TCustomIniFile; const ASection: string);
var
  AItem: TdxRibbonBackstageViewGalleryItem;
  I: Integer;
  Index: Integer;
begin
  AIniFile.EraseSection(ASection);
  AIniFile.WriteInteger(ASection, 'Count', AItems.Count);
  Index := 0;
  for I := AItems.Count - 1  downto 0 do
  begin
    AItem := AItems[I];
    AIniFile.WriteString(ASection, IntToStr(Index), AItem.Hint);
    AIniFile.WriteBool(ASection, IntToStr(Index) + 'Pinned', AItem.Pinned);
    Inc(Index);
  end;
end;

end.
