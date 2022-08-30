{*************************************************************************}
{ TDBSECTIONLISTBOX component                                             }
{ for Delphi & C++Builder                                                 }
{ version 1.0                                                             }
{                                                                         }
{ Copyright © 2001-2004                                                   }
{   TMS Software                                                          }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit dbslstbox;

interface

{$I TMSDEFS.INC}

uses
  SLstBox, DB, Classes, Graphics, PictureContainer, SysUtils, Windows;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TDBSectionListBox = class;

  TGetDataEvent = procedure (Sender:TObject; Section: TListSection;
    Tag:string;var Data:string) of object;

  TSectionDataLink = class(TDataLink)
  private
    FDBSectionListBox: TDBSectionListBox;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADBSectionListBox: TDBSectionListBox);
    destructor Destroy; override;
  end;

  TDBListSection = class(TListSection)
  private
    FDataLink: TSectionDataLink;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TDBListSectionCollection = class(TListSectionCollection)
  public
    function CreateItemClass: TCollectionItemClass; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBSectionListBox = class(TSectionListBox)
  private
    FOnGetData: TGetDataEvent;
    FOnTransformData: TGetDataEvent;
  protected
    function GetVersionNr: Integer; override;
    function CreateSections: TListSectionCollection; override;
    function GetDisplText(ListSection: TListSection;
      Index: Integer): string; override;
    function HTMLDBReplace(ListSection: TListSection;s:string; DataSet: TDataSet):string;
  public
  published
    property OnGetData: TGetDataEvent read FOnGetData write FOnGetData;
    property OnTransformData: TGetDataEvent read FOnTransformData write FOnTransformData;
  end;


implementation

{ TDBListSectionCollection }
function TDBListSectionCollection.CreateItemClass: TCollectionItemClass;
begin
  Result := TDBListSection;
end;

{ TDBSectionListBox }

function TDBSectionListBox.CreateSections: TListSectionCollection;
begin
  Result := TDBListSectionCollection.Create(Self);
  DoubleBuffered := True;
end;


function TDBSectionListBox.GetDisplText(ListSection: TListSection;
  Index: Integer): string;
var
  DText: string;
begin
  DText := Items[Index];

  with TDBListSection(ListSection) do
  begin
    if Assigned(Datasource) then
    begin
      if Assigned(DataSource.DataSet) then
      begin
        if DataSource.DataSet.Active then
        begin
          Result := HTMLDBReplace(ListSection,DText,Datasource.Dataset);
        end;
      end;
    end
    else
    begin
      Result := DText;
    end;
  end;
  UpdateHeight(Index);
end;


function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;


function TDBSectionListBox.HTMLDBReplace(ListSection: TListSection;
  s:string; DataSet: TDataSet):string;
var
  BeforeTag,AfterTag,Fld,DBFld:string;
  i,j,idx: Integer;
  ms: TMemoryStream;
  Picture: TPicture;
  FImageCache: THTMLPictureCache;

begin
  i := 1;

  FImageCache := GetImageCache;
  while i < FImageCache.Count do
  begin
    if FImageCache.Items[i - 1].IsDB then
    begin
      FImageCache.Items[i - 1].Free;
      FImageCache.Delete(i - 1);
    end
    else
      Inc(i);
  end;

  idx := 0;

  BeforeTag := '';
  while VarPos('<#',s,i)>0 do
  begin
    BeforeTag := BeforeTag + Copy(s,1,i - 1); //part prior to the tag
    AfterTag := Copy(s,i,Length(s)); //part after the tag

    j := Pos('>',AfterTag);
    Fld := Copy(AfterTag,1,j - 1);

    Delete(Fld,1,2);
    Delete(s,1,i + j - 1);

    DBFld := '';

    if Assigned(FOnGetData) then
      FOnGetData(Self,ListSection,Fld,DBFld);

    if Assigned(DataSet) and (DBFld = '') then
    begin
      if DataSet.Active then
      begin
        if Assigned(DataSet.FindField(Fld)) then
        if DataSet.FieldByName(Fld).IsBlob and
           (DataSet.FieldByName(fld).DataType in [ftGraphic,ftBytes,ftVarBytes]) then
        begin
          ms := TMemoryStream.Create;

          if DataSet.FieldByName(fld).DataType = ftGraphic then
          begin
            Picture := TPicture.Create;
            Picture.Assign(DataSet.FieldByName(fld));
            Picture.Graphic.SaveToStream(ms);
            Picture.Free;
          end
          else
            TBlobField(DataSet.FieldByName(fld)).SaveToStream(ms);

          ms.Position := 0;
          if ms.Size > 0 then
          with FImageCache.AddPicture do
          begin
            LoadFromStream(ms);
            ID := 'FILE://DB_' + IntToStr(idx);
            IsDB := true;
            inc(Idx);
            DBFld := ID;
          end;

          ms.Free;
        end
        else
          if DataSet.FieldByName(fld).DataType in [ftMemo,ftFmtMemo] then
            DBFld := DataSet.FieldByName(fld).AsString
          else
            DBFld := DataSet.FieldByName(fld).DisplayText;
      end
      else
        DBfld := '('+fld+')';
    end
    else
     if DBFld='' then DBfld := '('+fld+')';

    if Assigned(FOnTransformData) then
      FOnTransformData(Self,ListSection,Fld,DBFld);

    BeforeTag := BeforeTag + DBFld;
  end;

  Result := Beforetag + s;
end;

function TDBSectionListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TSectionDataLink }

procedure TSectionDataLink.ActiveChanged;
begin
  FDBSectionListBox.UpdateItemHeight;
end;

constructor TSectionDataLink.Create(ADBSectionListBox: TDBSectionListBox);
begin
  inherited Create;
  FDBSectionListBox := ADBSectionListBox;
end;

procedure TSectionDataLink.DataSetChanged;
begin
  FDBSectionListBox.UpdateItemHeight;
end;

procedure TSectionDataLink.DataSetScrolled(Distance: Integer);
begin
  FDBSectionListBox.UpdateItemHeight;
end;

destructor TSectionDataLink.Destroy;
begin
  inherited;
end;

procedure TSectionDataLink.RecordChanged(Field: TField);
begin
  FDBSectionListBox.UpdateItemHeight;
end;

{ TDBListSection }

constructor TDBListSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDataLink := TSectionDataLink.Create(
    TDBSectionListBox(TListSectionCollection(Collection).ListOwner));
end;

destructor TDBListSection.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TDBListSection.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TDBListSection.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;


end.
