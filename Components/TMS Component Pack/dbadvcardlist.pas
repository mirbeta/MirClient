{***************************************************************************}
{ TDBAdvCardList component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit DBAdvCardList;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Controls, Forms, Dialogs,
  DB, AdvCardList, JPeg, Themes, PictureContainer
  {$IFDEF DELPHI2010_LVL}
  , GIFImg, PNGImage
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.0 : New method InitTemplate to automatically initialize template according to DB fields
  // 1.1.1.0 : New event OnCardUpdate added
  // 1.2.0.0 : Compatibility update with TAdvCardList
  // 1.2.2.2 : Improved : shows DisplayText for string field types
  //         : Improved : editing DB updating
  // 1.2.2.3 : Fixed : issue with filtering tables
  // 1.5.0.0 : New : vertical scrollbar support added
  //         : New : themed checkbox support added
  // 2.0.0.0 : Update : See TAdvCardList history
  // 2.0.0.1 : Fixed : Issue with use in master/detail scenarios
  // 2.1.0.0 : New : HTML templates for card items
  // 2.1.1.0 : New : Event OnGetHTMLTemplate added
  // 2.1.2.0 : New : Use of FindField in HTML template handling instead of FieldByName

type
  { forward declarations }

  TDBAdvCardTemplateItem = class;
  TDBAdvCardTemplate = class;
  TDBAdvCardList = class;

  THTMLTemplateEvent = procedure(Sender: TObject; Card: TAdvCard; Item: TAdvCardItem; var Template: string) of object;

  { TDBAdvCardTemplateItem }

  TDBAdvCardTemplateItem = class(TAdvCardTemplateItem)
  private
    FField: TField;
    FFieldName: string;
    FHTMLTemplate: string;
    function GetField: TField;
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: String);
    procedure SetHTMLTemplate(const Value: string);
  protected
    function GetTemplate: TDBAdvCardTemplate;
    function GetCardList: TDBAdvCardList;
  public
    property CardList: TDBAdvCardList read GetCardList;
    property CardTemplate: TDBAdvCardTemplate read GetTemplate;
    property Field: TField read GetField write SetField;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
  end;

  { TAdvCardTemplateItems }

  TDBAdvCardTemplateItems = class(TAdvCardTemplateItems)
  private
    function GetCardTemplate: TDBAdvCardTemplate;
    function GetItem(Index: Integer): TDBAdvCardTemplateItem;
    procedure SetItem(Index: Integer; Value: TDBAdvCardTemplateItem);
  public
    function Add: TDBAdvCardTemplateItem;
    property CardTemplate: TDBAdvCardTemplate read GetCardTemplate;
    function GetItemByName(Name: string): TDBAdvCardTemplateItem;
    function Insert(Index: Integer): TDBAdvCardTemplateItem;
    property Items[Index: Integer]: TDBAdvCardTemplateItem read GetItem write SetItem; default;
  end;

  { TDBAdvCardTemplate }

  TDBAdvCardTemplate = class(TAdvCardTemplate)
  private
    FField: TField;
    FFieldName: string;
    function GetField: TField;
    function GetItems: TDBAdvCardTemplateItems;
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: String);
    procedure SetItems(Value: TDBAdvCardTemplateItems);
  protected
    function GetCardList: TDBAdvCardList;
  public
    constructor Create(CardList: TDBAdvCardList);
    property CardList: TDBAdvCardList read GetCardList;
    property CaptionField: TField read GetField write SetField;
  published
    property CaptionFieldName: String read FFieldName write SetFieldName;
    property Items: TDBAdvCardTemplateItems read GetItems write SetItems;
  end;

  { TCardListDataLink }

  TCardListDataLink = class(TDataLink)
  private
    FCardList: TDBAdvCardList;
    Adjusting: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(ACardList: TDBAdvCardList);
    procedure AdjustBuffer;
  end;

  THTMLTemplateDataEvent = procedure(Sender: TObject; Card: TAdvCard; Item: TAdvCardItem; AFieldName: string; var Data: string) of object;

  { TDBAdvCardList }
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvCardList = class(TCustomAdvCardList)
  private
    FDataLink: TCardListDataLink;
    CardsUpdating: Boolean;
    LockUpdate: Integer;
    Scrolling: Boolean;
    LastRecordRead: Boolean;
    FOnCardUpdate: TAdvCardEvent;
    FSelChange: boolean;
    FEditChange: boolean;
    FOnGetHTMLTemplate: THTMLTemplateEvent;
    FOnGetHTMLTemplateData: THTMLTemplateDataEvent;
    procedure CalcScrollBarPosition(var Pos: Integer);
    function GetCardTemplate: TDBAdvCardTemplate;
    function GetDataSource: TDataSource;
    procedure SetCardTemplate(Value: TDBAdvCardTemplate);
    procedure SetDataSource(Value: TDataSource);
    function GetBufferCount: Integer;
    procedure SetBufferCount(Value: Integer);
  protected
    procedure CreateTemplate(Cards: TAdvCards); override;
    procedure DataChanged(Card: TAdvCard; Item: TAdvCardItem; DataObject: TDataChangedObject); override;
    procedure RecordChanged(Field: TField);
    function GetVersionNr: Integer; override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LocateByChar(Key: Char); override;
    procedure SelectedChanged; override;
    procedure ColumnSized; override;
    procedure Resize; override;
    procedure UpdateItem(Card: TAdvCard; ItemIndex: Integer);
    procedure UpdateCard(Card: TAdvCard);
    procedure UpdateDBCards;
    procedure UpdateScrollBar; override;
    procedure OnScroll(var ScrollPos: Integer; ScrollCode: TScrollCode); override;
    function JumpToCard(Offset: Integer; ToBegin, ToEnd: Boolean): Boolean; override;
    function HTMLDBReplace(Card: TAdvCard; Item: TAdvCardItem; s: string; dataset: TDataSet): string;
    property DataLink: TCardListDataLink read FDataLink;
    property BufferCount: Integer read GetBufferCount write SetBufferCount default 5;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Columns;
    property Cards;
    function FindCard(BeginWith: string): TAdvCard; override;
    property LeftCol;
    procedure InitTemplate;
    procedure StartDBEdit;
    procedure StopDBEdit;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property CardTemplate: TDBAdvCardTemplate read GetCardTemplate write SetCardTemplate;
    //
    property Align;
    property Anchors;
    property Enabled;
    property TabOrder;
    property TabStop;
    property HelpContext;
    property DragKind;
    property DragCursor;
    property DragMode;
    property BiDiMode;
    property Constraints;
    property DockOrientation;
    property ShowHint;
    property Visible;
    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Hint;
    property PopupMenu;
    { TCustomAdvCardList }
    property AutoEdit;
    property BorderColor;
    property BorderWidth;
    property CardEditingAppearance;
    property CardNormalAppearance;
    property CardSelectedAppearance;
    property CardHoverAppearance;
    property CardHorSpacing;
    property CardVertSpacing;
    property Color;
    property ColumnSizing;
    property ColumnWidth;
    property DelayedCardLoad;
    property DelayedCardLoadInterval;
    property FocusColor;
    property GotoSelectedAutomatic;    
    property GridLineColor;
    property GridLineWidth;
    property Images;
    property MaxColumnWidth;
    property MinColumnWidth;
    property MultiSelect;
    property PageCount;
    property ReadOnly;
    property ScrollBarType;
    property SelectedCount;
    property ShowGridLine;
    property ShowFocus;
    property ShowScrollBar;
    property URLColor;
    property Version;
    { events }
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    { TCustomAdvCardList }
    property OnCardStartEdit;
    property OnCardEndEdit;
    property OnCardCaptionGetDisplText;
    property OnCardItemGetDisplText;
    property OnCardCaptionClick;
    property OnCardCaptionDblClick;
    property OnCardClick;
    property OnCardComboSelect;
    property OnCardDblClick;
    property OnCardItemClick;
    property OnCardItemURLClick;

    property OnCardUpdate: TAdvCardEvent read FOnCardUpdate write FOnCardUpdate;
    property OnColumnResizing;
    property OnDrawCardItem;
    property OnDrawCardItemProp;
    property OnGetHTMLTemplate: THTMLTemplateEvent read FOnGetHTMLTemplate write FOnGetHTMLTemplate;
    property OnGetHTMLTemplateData: THTMLTemplateDataEvent read FOnGetHTMLTemplateData write FOnGetHTMLTemplateData;
    property OnShowCardItem;
  end;

implementation

{$IFDEF DELPHIXE4_LVL}
uses
  AnsiStrings;
{$ENDIF}

type
  TAdvCardItemEx = class(TAdvCardItem);

const
  MinGraphicSize = 44;


//------------------------------------------------------------------------------

{$IFDEF DELPHIXE2_LVL}
function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

function ThemeServicesThemesEnabled: boolean;
begin
  Result := StyleServices.Enabled;
end;
{$ENDIF}

{$IFNDEF DELPHIXE2_LVL}
function ThemeServicesThemesEnabled: boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;
{$ENDIF}

//------------------------------------------------------------------------------


//----------------------------//
//   TDBAdvCardTemplateItem   //
//----------------------------//

function TDBAdvCardTemplateItem.GetField: TField;
var
  CardList: TDBAdvCardList;
begin
  CardList := GetCardList;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(CardList) and
    Assigned(CardList.DataLink.DataSet) then
  with CardList.Datalink.Dataset do
  begin
  {$IFDEF DELPHIXE6_LVL}
    if Active or not (lcAutomatic in Fields.LifeCycles) then
  {$ELSE}
    if Active or (not DefaultFields) then
  {$ENDIF}
      SetField(FindField(FieldName));
  end;

  Result := FField;
end;

procedure TDBAdvCardTemplateItem.SetField(Value: TField);
var
  CardList: TDBAdvCardList;
begin
  if FField = Value then
    Exit;

  if Assigned(FField) and (GetCardList <> nil) then
    FField.RemoveFreeNotification(GetCardList);

  FField := Value;
  CardList := GetCardList;
  if Assigned(Value) then
  begin
    if CardList <> nil then
      FField.FreeNotification(GetCardList);
    FFieldName := Value.FullName;
  end;

  if Assigned(CardList) then
    CardList.DataLink.RecordChanged(nil);
end;

procedure TDBAdvCardTemplateItem.SetFieldName(const Value: String);
var
  AField: TField;
  CardList: TDBAdvCardList;
begin
  AField := nil;
  CardList := GetCardList;
  if Assigned(CardList) and Assigned(CardList.DataLink.DataSet) and
    not (csLoading in CardList.ComponentState) and (Length(Value) > 0) then
      AField := CardList.DataLink.DataSet.FindField(Value);
  FFieldName := Value;
  SetField(AField);
end;

procedure TDBAdvCardTemplateItem.SetHTMLTemplate(const Value: string);
begin
  if (FHTMLTemplate <> Value) then
  begin
    FHTMLTemplate := Value;
    CardList.UpdateDBCards;
  end;
end;

function TDBAdvCardTemplateItem.GetCardList: TDBAdvCardList;
var
  Template: TDBAdvCardTemplate;
begin
  Template := GetTemplate;
  if Assigned(Template) then
    Result := Template.CardList
  else
    Result := nil;
end;

function TDBAdvCardTemplateItem.GetTemplate: TDBAdvCardTemplate;
begin
  if Assigned(Collection) and (Collection is TDBAdvCardTemplateItems) then
    Result := TDBAdvCardTemplateItems(Collection).CardTemplate
  else
    Result := nil;
end;

//-----------------------------//
//   TDBAdvCardTemplateItems   //
//-----------------------------//

function TDBAdvCardTemplateItems.GetCardTemplate: TDBAdvCardTemplate;
begin
  Result := TDBAdvCardTemplate(inherited CardTemplate);
end;

function TDBAdvCardTemplateItems.GetItem(Index: Integer): TDBAdvCardTemplateItem;
begin
  Result := TDBAdvCardTemplateItem(inherited GetItem(Index));
end;

procedure TDBAdvCardTemplateItems.SetItem(Index: Integer; Value: TDBAdvCardTemplateItem);
begin
  inherited SetItem(Index, Value);
end;

function TDBAdvCardTemplateItems.Add: TDBAdvCardTemplateItem;
begin
  Result := TDBAdvCardTemplateItem(inherited Add);
end;

function TDBAdvCardTemplateItems.GetItemByName(Name: string): TDBAdvCardTemplateItem;
begin
  Result := TDBAdvCardTemplateItem(inherited GetItemByName(Name));
end;

function TDBAdvCardTemplateItems.Insert(Index: Integer): TDBAdvCardTemplateItem;
begin
  result := TDBAdvCardTemplateItem(inherited Insert(Index));
end;

//------------------------//
//   TDBAdvCardTemplate   //
//--------------------- --//

constructor TDBAdvCardTemplate.Create(CardList: TDBAdvCardList);
begin
  inherited Create(CardList, TDBAdvCardTemplateItems, TDBAdvCardTemplateItem);
  FField := nil;
end;

function TDBAdvCardTemplate.GetField: TField;
var
  CardList: TDBAdvCardList;
begin
  CardList := GetCardList;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(CardList) and
    Assigned(CardList.DataLink.DataSet) then
  with CardList.Datalink.Dataset do
  {$IFDEF DELPHIXE6_LVL}
    if Active or not (lcAutomatic in Fields.LifeCycles) then
  {$ELSE}
    if Active or (not DefaultFields) then
  {$ENDIF}
      SetField(FindField(CaptionFieldName));
  Result := FField;
end;

function TDBAdvCardTemplate.GetItems: TDBAdvCardTemplateItems;
begin
  Result := TDBAdvCardTemplateItems(inherited Items);
end;

procedure TDBAdvCardTemplate.SetField(Value: TField);
var
  CardList: TDBAdvCardList;
begin
  if FField = Value then Exit;

  if Assigned(FField) and (GetCardList <> nil) then
    FField.RemoveFreeNotification(GetCardList);

  FField := Value;
  CardList := GetCardList;
  if Assigned(Value) then
  begin
    if CardList <> nil then
      FField.FreeNotification(GetCardList);
    FFieldName := Value.FullName;
  end;
  if Assigned(CardList) then
    CardList.DataLink.RecordChanged(nil);
end;

procedure TDBAdvCardTemplate.SetFieldName(const Value: String);
var
  AField: TField;
  CardList: TDBAdvCardList;
begin
  AField := nil;
  CardList := GetCardList;
  if Assigned(CardList) and Assigned(CardList.DataLink.DataSet) and
    not (csLoading in CardList.ComponentState) and (Length(Value) > 0) then
      AField := CardList.DataLink.DataSet.FindField(Value);
  FFieldName := Value;
  SetField(AField);
end;

procedure TDBAdvCardTemplate.SetItems(Value: TDBAdvCardTemplateItems);
begin
  inherited Items := Value;
end;

function TDBAdvCardTemplate.GetCardList: TDBAdvCardList;
begin
  Result := TDBAdvCardList(inherited CardList);
end;

//-----------------------//
//   TCardListDataLink   //
//-----------------------//

constructor TCardListDataLink.Create(ACardList: TDBAdvCardList);
begin
  inherited Create;
  VisualControl := True;
  FCardList := ACardList;
  Adjusting := False;
end;

procedure TCardListDataLink.ActiveChanged;
begin
  if Assigned(FCardList) then FCardList.LinkActive(Active);
end;

procedure TCardListDataLink.AdjustBuffer;
var                                                   
  MB: Integer;
begin
  if Adjusting then Exit;
  Adjusting := True;
  if (FCardList.Columns <= FCardList.LeftCol + FCardList.VisibleColumns) and
     (DataSet.RecordCount <> RecordCount) and
     not EOF and not BOF and not FCardList.LastRecordRead then
  begin
    MB := RecordCount - ActiveRecord + RecordCount - FCardList.VisibleCardCount - 1;
    if MB <> 0 then
    begin
      MoveBy(-MoveBy(MB));
      FCardList.UpdateDBCards;
    end;
  end;
  Adjusting := False;
end;

procedure TCardListDataLink.DataSetChanged;
begin
  inherited;
end;

procedure TCardListDataLink.DataSetScrolled(Distance: Integer);
var
  DoAdjust: Boolean;
begin
  if not Assigned(FCardList) then
    Exit;

  if FCardList.FSelChange then
    Exit;

  if FCardList.FEditChange then
    Exit;

  FCardList.Scrolling := True;

  if (FCardList.SelectedIndex + Distance * RecordCount < 0) or
     (FCardList.SelectedIndex + Distance * RecordCount > FCardList.Cards.Count - 1) then
  begin
    FCardList.UpdateDBCards;
  end
  else
  begin
    inc(FCardList.LockUpdate);
    //DoAdjust := ActiveRecord >= FCardList.SelectedIndex;
    DoAdjust := (ActiveRecord > FCardList.VisibleCardCount - 1) or (ActiveRecord = 0);
    FCardList.SelectedIndex := ActiveRecord;
    if DoAdjust then
      AdjustBuffer;
    dec(FCardList.LockUpdate);
  end;

  FCardList.UpdateScrollBar;

  FCardList.Scrolling := False;
end;

procedure TCardListDataLink.EditingChanged;
begin
  inherited;
end;

procedure TCardListDataLink.RecordChanged(Field: TField);
begin
  if not Assigned(FCardList) then
    Exit;
  FCardList.RecordChanged(Field);
end;

//--------------------//
//   TDBAdvCardList   //
//--------------------//

constructor TDBAdvCardList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CardsUpdating := False;
  LockUpdate := 0;
  Filtered := False;
  Sorted := False;
  FDataLink := TCardListDataLink.Create(Self);
  FDataLink.BufferCount := 5;
end;

destructor TDBAdvCardList.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TDBAdvCardList.CreateTemplate(Cards: TAdvCards);
begin
  FCardTemplate := TDBAdvCardTemplate.Create(Self);
  FCardTemplate.Items.Cards := Cards;
end;

procedure TDBAdvCardList.DataChanged(Card: TAdvCard; Item: TAdvCardItem; DataObject: TDataChangedObject);
var
  OldSelItem: Integer;

begin
  if not Assigned(FDataLink) or (csLoading in ComponentState) then Exit;

  FEditChange := true;

  if FDataLink.Active and not CardsUpdating
     and not (FDataLink.DataSet.State in [dsEdit])
     and (LockUpdate <= 0) then
  begin
    OldSelItem := Item.Index;

    if DataObject = dcoItem then
    begin
      if Assigned(CardTemplate.Items[Item.Index].Field) then
      begin
        inc(LockUpdate);

        try
          if SelectedIndex <> Card.Index then
            SelectedIndex := Card.Index;

          if FDataLink.Edit then
          begin
            try
              case CardTemplate.Items[Item.Index].DataType of
                idtFloat: CardTemplate.Items[Item.Index].Field.AsFloat := Item.AsFloat;
                idtInteger: CardTemplate.Items[Item.Index].Field.AsInteger := Item.AsInteger;
                idtDate: CardTemplate.Items[Item.Index].Field.AsDateTime := Item.AsDate;
                idtTime: CardTemplate.Items[Item.Index].Field.AsDateTime := Item.AsTime;
                idtImage: CardTemplate.Items[Item.Index].Field.Assign(Item.Picture);
                idtBoolean: CardTemplate.Items[Item.Index].Field.AsBoolean := Item.AsBoolean;
              else CardTemplate.Items[Item.Index].Field.AsString := Item.AsString;
              end;
            except
              FDataLink.DataSet.Cancel;
              UpdateItem(Card, Item.Index);
            end;
          end;
        finally
          dec(LockUpdate);
        end;
      end;
    end
    else
    if DataObject = dcoCaption then
    begin
      if Assigned(CardTemplate.CaptionField) then
      begin
        inc(LockUpdate);
        try
          if CardTemplate.CaptionField.AsString <> Card.Caption then
            if FDataLink.Edit then
            begin
              try
                CardTemplate.CaptionField.AsString := Card.Caption;
              except
                if Assigned(CardTemplate.CaptionField) then
                  Card.Caption := CardTemplate.CaptionField.AsString;
              end;
            end;
        finally
          dec(LockUpdate);
        end;
      end;
    end;
    try
      if FDataLink.Edit then FDataLink.DataSet.Post;
    except
      FDataLink.DataSet.Cancel;
      FEditChange := false;
    end;

    if not FEditChange then
    begin
      UpdateDBCards;
      if SelectedIndex <> -1 then
        SelectedCard.SelectedItem := OldSelItem;
    end;
  end;

  FEditChange := false;
end;

function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then
    Exit;

  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    {$IFDEF DELPHIXE_LVL}
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
    {$ENDIF}
  else
    {$IFDEF DELPHI2010_LVL}
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else
    {$ENDIF}
    if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    {$IFDEF DELPHI2010_LVL}
    {$IFDEF DELPHIXE4_LVL}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
      GraphicClass := TGIFImage
    {$ENDIF}
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;


function LoadPictureFromBlobField(Field: TBlobField; Dest: THTMLPicture): boolean;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;

begin
  Graphic := nil;
  Result := false;

  Stream := TMemoryStream.Create;
  try
    Field.SaveToStream(Stream);
    if Stream.Size = 0 then
    begin
      Dest.Assign(nil);
      Exit;
    end;
    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
    begin
      Exit;
    end
    else
    begin
      Graphic := GraphicClass.Create;
      Stream.Position := 0;
      Graphic.LoadFromStream(Stream);
      Dest.Assign(Graphic);
      Result := true;
    end;
  finally
    Stream.Free;
    Graphic.Free;
  end;
end;

function TDBAdvCardList.HTMLDBReplace(Card: TAdvCard; Item: TAdvCardItem; s: string; Dataset: TDataSet): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i,j,k: integer;
  afield: TField;
  piccont: TPictureContainer;
  picitem: TPictureItem;
  pic: THTMLPicture;
begin
  beforetag := '';
  TAdvCardItemEx(Item).PictureContainer.Items.Clear;

  piccont := TAdvCardItemEx(Item).PictureContainer;

  k := 0;
  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        afield := DataSet.FindField(fld);

        if Assigned(afield) then
        begin
          if afield.IsBlob then
          begin
            // try to access blob as image
            picitem := piccont.Items.Add;
            picitem.Name := 'image'+ inttostr(k);
            pic := picitem.Picture as THTMLPicture;
            if LoadPictureFromBlobField(AField as TBlobField, pic) then
            begin
              dbfld := '<IMG src="'+ picitem.Name+'">';
              inc(k);
            end
            else
            begin
              piccont.Items.Delete(piccont.Items.Count - 1);
              if Assigned(aField.OnGetText) then
                dbfld := aField.DisplayText
              else
                dbfld := aField.AsString;
            end;
          end
          else
           dbfld := afield.DisplayText;
        end;

        if Assigned(FOnGetHTMLTemplateData) then
          FOnGetHTMLTemplateData(self, Card, Item, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;


procedure TDBAdvCardList.RecordChanged(Field: TField);
begin
  if not HandleAllocated or (LockUpdate > 0) then Exit;

  if FEditChange then
    Exit;

  inc(LockUpdate);

  try
    if (Field = nil) or (SelectedIndex < 0) then
      UpdateDBCards
    else
    if Assigned(SelectedCard) then
    begin
      UpdateCard(SelectedCard);
    end;
  finally
    dec(LockUpdate);
  end;
end;

function TDBAdvCardList.GetCardTemplate: TDBAdvCardTemplate;
begin
  Result := TDBAdvCardTemplate(FCardTemplate);
end;

function TDBAdvCardList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvCardList.SetCardTemplate(Value: TDBAdvCardTemplate);
begin
  TDBAdvCardTemplate(FCardTemplate).Assign(Value);
end;

procedure TDBAdvCardList.InitTemplate;
var
  i: Integer;
  fld: TField;
  CTItem: TDBAdvCardTemplateItem;
begin
  if FDataLink.DataSource.DataSet.FieldCount > 0 then
    CardTemplate.Items.Clear
  else
    Exit;

  inc(LockUpdate);

  for i := 1 to FDataLink.DataSource.DataSet.FieldCount do
  begin
    fld := FDataLink.DataSource.DataSet.Fields[i - 1];

    CTItem := nil;

    if fld.DataType in [ftString, ftInteger, ftSmallInt, ftLargeInt, ftCurrency, ftWord, ftFloat, ftDate, ftDateTime,
      ftTime,{$IFDEF DELPHI6_LVL} ftTimeStamp,{$ENDIF} ftDateTime, ftBoolean, ftGraphic, ftMemo] then
    begin
      CTItem := TDBAdvCardTemplateItem( CardTemplate.Items.Add);
      CTItem.AssignVisuals(CardTemplate.DefaultItem);
      CTItem.FieldName := fld.FieldName;
      CTItem.ReadOnly := fld.ReadOnly;
    end;

    case fld.DataType of
    ftString:
      begin
        CTItem.ItemEditor := ieText;
        CTItem.DataType := idtString;
      end;
    ftInteger, ftWord, ftSmallInt, ftLargeInt:
      begin
        CTItem.ItemEditor := ieNumber;
        CTItem.DataType := idtInteger;
      end;
    ftFloat, ftCurrency:
      begin
        CTItem.ItemEditor := ieFloat;
        CTItem.DataType := idtFloat;
      end;
    ftDate, ftDateTime:
      begin
        CTItem.ItemEditor := ieDate;
        CTItem.DataType := idtDate;
      end;
    ftTime {$IFDEF DELPHI6_LVL}, ftTimeStamp{$ENDIF}:
      begin
        CTItem.ItemEditor := ieTime;
        CTItem.DataType := idtTime;
      end;
    ftBoolean:
      begin
        CTItem.DataType := idtBoolean;
        CTItem.ItemEditor := ieBoolean;
      end;
    ftGraphic:
      begin
        CTItem.ItemEditor := iePictureDialog;
        CTItem.DataType := idtImage;
      end;
    ftMemo:
      begin
        CTItem.ItemEditor := ieText;
        CTItem.DataType := idtString;
        CTItem.WordWrap := true;
      end;
    end;
  end;
  
  dec(LockUpdate);
  RecordChanged(nil);
end;

procedure TDBAdvCardList.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then
    Exit;

  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);

  if (csDesigning in ComponentState) and Assigned(FDataLink.DataSource) then
  begin
    if Assigned(FDataLink.DataSource.DataSet) then
    begin
      if FDataLink.DataSource.DataSet.Active then
        if (MessageDlg('Automatically add  template items for dataset fields ?',mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          InitTemplate;
    end;
  end;
end;

procedure TDBAdvCardList.StartDBEdit;
begin
  FEditChange := true;
end;

procedure TDBAdvCardList.StopDBEdit;
begin
  FEditChange := false;
end;

function TDBAdvCardList.GetBufferCount: Integer;
begin
  Result := 0;
  if Assigned(FDataLink) then Result := FDataLink.BufferCount;
end;

procedure TDBAdvCardList.SetBufferCount(Value: Integer);
begin
  if Assigned(FDataLink) then
  begin
    FDataLink.BufferCount := Value;
    UpdateDBCards;
  end;
end;

function TDBAdvCardList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TDBAdvCardList.SelectedChanged;
var
  MoveCount: Integer;
begin
  if CardsUpdating or (LockUpdate > 0) then Exit;
  if Assigned(FDatalink) and FDatalink.Active and (SelectedIndex <> -1) then
    with FDataLink.DataSet do
      begin
        FSelChange := true;
        inc(LockUpdate);
        MoveCount := SelectedIndex - FDataLink.ActiveRecord;
        FDataLink.MoveBy(MoveCount);
        dec(LockUpdate);
        FSelChange := false;
      end;
end;

procedure TDBAdvCardList.ColumnSized;
begin
  UpdateDBCards;
end;

procedure TDBAdvCardList.Resize;
begin
  inherited;
  UpdateDBCards;
end;

procedure TDBAdvCardList.UpdateItem(Card: TAdvCard; ItemIndex: Integer);
var
  TItem: TDBAdvCardTemplateItem;
  APic2: TPicture;
  P: TPoint;
  templ: string;

  procedure BlobFieldToStream(DBField: TBlobField; var size: tpoint; APic: TPicture);
  var
    s, ms: TMemoryStream;
    sig: word;
    b: byte;
    Ljpg: TJPEGImage;
    ABitmap: TBitmap;
    APicture: TPicture;
    oletype: integer;
    oleoffset: integer;
    i: Integer;

  begin
    size.X := 0;
    size.Y := 0;

    if (TBlobField(DBField).BlobType = ftGraphic) or (1 > 0) then
    begin
      s := TMemoryStream.Create;
      try
        DBField.SaveToStream(S);

        oletype := -1;
        oleoffset := 0;

        if s.Size > 2 then
        begin
      // find file type
          S.Position := 0;
          S.Read(sig, 2);

          case sig of
            $1C15: // OLE storage
              begin
                i := 0;
                while (i < 512) do
                begin
                  S.Read(b, 1);
                  inc(i);
                  if (b = $FF) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if b = $D8 then
                    begin
                      oletype := 1;
                      oleoffset := i;
                      break;
                    end;
                  end;
                  if (b = $47) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if b = $49 then
                    begin
                      oletype := 2;
                      oleoffset := i;
                      break;
                    end;
                  end;
                  if (b = ord('B')) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if (b = ord('M')) then
                    begin
                      oletype := 0;
                      oleoffset := i;
                      Break;
                    end;
                  end;
                end;

                S.Seek(oleoffset, 0);


                case oletype of
                  0:
                    begin
                      ABitmap := TBitmap.Create;
                      ABitmap.LoadFromStream(S);
                      if not ABitmap.Empty then
                      begin
                        APic.Assign(ABitmap);

                        size.X := ABitmap.Width;
                        size.Y := ABitmap.Height;
                      end;
                      ABitmap.Free;
                    end;
                  1:
                    begin
                      LJPg := TJPEGImage.Create;
                      try
                        LJpg.LoadFromStream(S);
                        APicture := TPicture.Create;
                        APicture.Assign(LJpg);

                        if not APicture.Graphic.Empty then
                        begin
                          APic.Assign(APicture);

                          size.X := APicture.Graphic.Width;
                          size.Y := APicture.Graphic.Height;
                        end;
                        APicture.Free;
                      finally
                        FreeAndNil(LJpg);
                      end;
                    end;
                  2:
                    begin
                      ms := TMemoryStream.Create;
                      ms.CopyFrom(s, s.Size - s.Position);

                      APic.Graphic.LoadFromStream(ms);

                      ms.Free;
                    end;
                end;
              end;
            $4947: //gif signature
              begin
                ms := TMemoryStream.Create;
                s.Position := 0;
                ms.LoadFromStream(s);

                APic.Graphic.LoadFromStream(ms);

                ms.Free;
              end;
            $D8FF: //jpeg signature
              begin
                S.Position := 0;
                LJPg := TJPEGImage.Create;
                try
                  LJpg.LoadFromStream(S);
                  APicture := TPicture.Create;
                  APicture.Assign(LJpg);

                  if not APicture.Graphic.Empty then
                  begin
                    APic.Assign(APicture);

                    size.X := APicture.Graphic.Width;
                    size.Y := APicture.Graphic.Height;
                  end;
                  APicture.Free;
                finally
                  FreeAndNil(LJpg);
                end;
              end
          else
            begin
              APicture := TPicture.Create;
              APicture.Assign(DBField);
              if not APicture.Graphic.Empty then
              begin
                APic.Assign(APicture);

                size.X := APicture.Graphic.Width;
                size.Y := APicture.Graphic.Height;
                APicture.Free;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(S);
      end;
    end;
  end;

begin
  TItem := CardTemplate.Items[ItemIndex];

  if TItem.ItemType = itHTMLText then
  begin
    templ := TItem.HTMLTemplate;
    if Assigned(OnGetHTMLTemplate) then
      OnGetHTMLTemplate(Self, Card, Card.ItemList[ItemIndex], templ);

    Card.ItemList[ItemIndex].AsString := HTMLDBReplace(Card, Card.ItemList[ItemIndex], templ, DataSource.DataSet);
  end
  else
  if Assigned(TItem.Field) then
  begin
    case TItem.DataType of
      idtString:
        begin
          Card.ItemList[ItemIndex].AsString := AdjustLineBreaks(TItem.Field.DisplayText);
        end;
      idtInteger: Card.ItemList[ItemIndex].AsInteger := TItem.Field.AsInteger;
      idtFloat: Card.ItemList[ItemIndex].AsFloat := TItem.Field.AsFloat;
      idtDate: Card.ItemList[ItemIndex].AsDate := TItem.Field.AsDateTime;
      idtTime: Card.ItemList[ItemIndex].AsTime := TItem.Field.AsDateTime;
      idtImage:
      begin
       { if (TBlobField(TItem.Field).BlobType = ftGraphic) then
          Card.ItemList[ItemIndex].Picture.Assign(TItem.Field)
        else }
        begin
          APic2 := TPicture.Create;
          BlobFieldToStream(TBlobField(TItem.Field), p, APic2);
          Card.ItemList[ItemIndex].Picture.Assign(APic2);
          APic2.Free;
        end;
      end;
      idtBoolean: Card.ItemList[ItemIndex].AsBoolean := TItem.Field.AsBoolean;
    end;
  end;
  Cards.CheckItemShow(Card, ItemIndex);
end;

procedure TDBAdvCardList.UpdateCard(Card: TAdvCard);
var
  ItemN: Integer;
begin
  if Assigned(CardTemplate.CaptionField) then
    Card.Caption := CardTemplate.CaptionField.AsString;
    
  for ItemN := 0 to CardTemplate.Items.Count - 1 do
    UpdateItem(Card, ItemN);

  if Assigned(FOnCardUpdate) then
    FOnCardUpdate(Self, Card);
end;

procedure TDBAdvCardList.UpdateDBCards;
var
  CardN, OldActive, MaxVisi: Integer;
label
  _Cont;
begin
  if CardsUpdating or not FDataLink.Active then
    Exit;

  MaxVisi := 0;

  if FDataLink.RecordCount = 0 then
  begin
    Cards.Clear;
    Exit;
  end;

_Cont:

  BeginUpdate;
  CardsUpdating := True;

  Cards.Clear;
  OldActive := FDataLink.ActiveRecord;
  LastRecordRead := False;
  CardN := 0;

  while (CardN < FDataLink.RecordCount) do
  begin
    FDataLink.ActiveRecord := CardN;
    if FDataLink.Eof then
      LastRecordRead := True;

    UpdateCard(Cards.Add);
    Inc(CardN);
  end;

  if (OldActive <= FDataLink.RecordCount - 1) and (OldActive >= 0) then
    FDataLink.ActiveRecord := OldActive
  else
  begin
    if (FDataLink.RecordCount >= 0) then
      FDataLink.ActiveRecord := FDataLink.RecordCount - 1;
  end;

  DoSort(False);
  SelectedIndex := FDataLink.ActiveRecord;

  CardsUpdating := False;
  EndUpdate;

  if (VisibleCardCount >= MaxVisi) and (VisibleCardCount > 0) then
  begin
    MaxVisi := VisibleCardCount;

    if (VisibleCardCount = FDataLink.BufferCount) and not FDataLink.Eof then
    begin
      FDataLink.BufferCount := FDataLink.BufferCount + 1;
      goto _Cont;
    end
    else
    if (FDataLink.BufferCount > VisibleCardCount + 2) and not LastRecordRead then
    begin
      FDataLink.BufferCount := VisibleCardCount + 2;
      goto _Cont;
    end;
  end;

  FDataLink.AdjustBuffer;
end;

procedure TDBAdvCardList.CalcScrollBarPosition(var Pos: Integer);
begin
  with FDatalink.DataSet do
  begin
    if IsSequenced then
    begin
      if State in [dsInactive, dsBrowse, dsEdit] then Pos := RecNo;
    end
    else
    begin
      if FDataLink.BOF then Pos := 0
      else if FDataLink.EOF then Pos := 4
      else Pos := 2;
    end;
  end;
end;

procedure TDBAdvCardList.UpdateScrollBar;
var
  Pos, ScrMax: Integer;
  OldFocus: Boolean;
begin
{$IFDEF DELPHI7_LVL}
  OldFocus := false;
{$ENDIF}

  if not Assigned(FDataLink) or (csLoading in ComponentState) or
    (csDesigning in ComponentState) then Exit;
  if FDatalink.Active and HandleAllocated and ShowScrollbar then
  begin
{$IFDEF DELPHI7_LVL}
    if not ThemeServicesThemesEnabled then
{$ENDIF}
    begin
      OldFocus := ScrollBar.Focused;
      if OldFocus then ScrollBar.Enabled := False;
    end;
    with FDatalink.DataSet do
    begin
      if IsSequenced then
      begin
        ScrollBar.Min := 1;
        ScrollBar.PageSize := 1;
        ScrMax := RecordCount;
        if ScrMax < ScrollBar.Min then ScrMax := ScrollBar.Min;
        ScrollBar.Max := ScrMax;
        CalcScrollBarPosition(Pos);
        if Pos < ScrollBar.Min then Pos := ScrollBar.Min;
        if Pos > ScrollBar.Max then Pos := ScrollBar.Max;
        if Eof then Pos := ScrollBar.Max;
        ScrollBar.Position := Pos;
      end
      else
      begin
        ScrollBar.Min := 0;
        ScrollBar.PageSize := 0;
        ScrollBar.Max := 4;
        CalcScrollBarPosition(Pos);
        ScrollBar.Position := Pos;
      end;
      ScrollBar.Visible := True;
    end;
{$IFDEF DELPHI7_LVL}
    if not ThemeServicesThemesEnabled then
{$ENDIF}
      if OldFocus then
      begin
        ScrollBar.Enabled := True;
        if ScrollBar.Visible then ScrollBar.SetFocus;
      end;
  end else ScrollBar.Visible := False;
end;

procedure TDBAdvCardList.OnScroll(var ScrollPos: Integer; ScrollCode: TScrollCode);
begin
  if Scrolling or not Assigned(FDataLink) or (csLoading in ComponentState) or
    (csDesigning in ComponentState) then Exit;
    
  Scrolling := True;
  if Assigned(FDatalink) and FDatalink.Active then
    with FDataLink.DataSet do
    case ScrollCode of
      scLineUp: FDataLink.MoveBy(-1);
      scLineDown: FDataLink.MoveBy(1);
      scPageUp:
        begin
          FDataLink.MoveBy(-PageCount);
        end;
      scPageDown:
        begin
          FDataLink.MoveBy(PageCount);
        end;
      scPosition:
        if IsSequenced then
        begin
          if ScrollPos <= 1 then First
          else if ScrollPos >= RecordCount then Last
          else RecNo := ScrollPos;
        end
        else
          case ScrollPos of
            0: First;
            1: FDataLink.MoveBy( -(VisibleCardCount - 1));
            2: ;
            3: FDataLink.MoveBy( VisibleCardCount - 1);
            4: Last;
          end;
      scBottom: Last;
      scTop: First;
    end;
  CalcScrollBarPosition(ScrollPos);
  Scrolling := False;
end;

function TDBAdvCardList.JumpToCard(Offset: Integer; ToBegin, ToEnd: Boolean): Boolean;
begin
  if ToBegin then
  begin
    if FDataLink.Active then
    begin
      FDataLink.DataSet.First;
      Result := True;
    end
    else
      Result := inherited JumpToCard(Offset, ToBegin, ToEnd);
  end {ToBegin}
  else
    if ToEnd then
    begin
      if FDataLink.Active then
      begin
        FDataLink.DataSet.Last;
        Result := True;
      end
      else
        Result := inherited JumpToCard(Offset, ToBegin, ToEnd);
    end {ToEnd}
    else
    begin
      if FDataLink.Active then
      begin
        Result := FDataLink.MoveBy(Offset) <> 0;
      end
      else
        Result := inherited JumpToCard(Offset, ToBegin, ToEnd);
    end;
end;

procedure TDBAdvCardList.LinkActive(Value: Boolean);
var
  ItemN: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Value then
    begin
      UpdateDBCards;
    end
    else
    begin
      Cards.Clear;
      CardTemplate.FField := nil;
      for ItemN := 0 to CardTemplate.Items.Count - 1 do
        CardTemplate.Items[ItemN].FField := nil;
    end;
  end;
end;

procedure TDBAdvCardList.LocateByChar(Key: Char);
var
  ItemN, FromItemN: Integer;
  Item: TAdvCardItem;
  OldRecNo: Integer;

  function ItemLocate(ItemN: Integer): Boolean;
  begin
    Result := False;
    if FDataLink.DataSet.Locate(CardTemplate.Items[ItemN].FieldName,
       Key, [loCaseInsensitive, loPartialKey]) then
       begin
         SelectedCard.SelectedItem := ItemN;
         Result := True;
       end;
  end;

begin
  if Cards.Count = 0 then Exit;

  if not FDataLink.Active then
  begin
    inherited;
    Exit;
  end;

  if not Editing then
  begin

    if (SelectedIndex >= 0) then
    begin
      // search in items
      FromItemN := SelectedCard.SelectedItem + 1;
      for ItemN := FromItemN to SelectedCard.ItemList.Count - 1 do
      begin
        Item := SelectedCard.ItemList[ItemN];
        if not Item.Hided and CardTemplate.Items[ItemN].Visible and
          (Pos(AnsiUpperCase(Key), AnsiUpperCase(Item.AsString)) = 1) then
        begin
          SelectedCard.SelectedItem := ItemN;
          Exit;
        end;
      end;
    end;

    OldRecNo := FDataLink.ActiveRecord;

    try
      // Locate by Caption Field
      if CardTemplate.FField <> nil then
        if FDataLink.DataSet.Locate(CardTemplate.CaptionFieldName,
            Key, [loCaseInsensitive, loPartialKey]) then
          if FDataLink.ActiveRecord <> OldRecNo then Exit;
    except
    end;

    try
      // Locate by Item Fields
      for ItemN := 0 to CardTemplate.Items.Count - 1 do
        if CardTemplate.Items[ItemN].FField <> nil then
        begin
          ItemLocate(ItemN);
        end;
    except
    end;

    inherited;
  end; {if not Editing}
end;

function TDBAdvCardList.FindCard(BeginWith: string): TAdvCard;
begin
  Result := nil;

  if FDataLink.Active then
  begin
    if BeginWith = '' then
    begin
      FDataLink.DataSet.First;
      Result := SelectedCard;
      Exit;
    end;
    try
      // Locate by Caption Field
      if CardTemplate.FField <> nil then
        if FDataLink.DataSet.Locate(CardTemplate.CaptionFieldName,
            BeginWith, [loCaseInsensitive, loPartialKey]) then Result := SelectedCard;
    except
    end;
  end;
end;

end.
