{************************************************************************}
{ TDBAdvRichEditor                                                       }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2015                                            }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit DBAdvRichEditor;

interface

uses
  Classes, AdvRichEditor, AdvRichEditorBase, DB, DBCtrls;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvRichEditor = class(TAdvRichEditor)
  private
    FDataLink: TFieldDataLink;
    FDBUpdate: boolean;
    FMergeSource: TDataSource;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetMergeSource: TDataSource;
    procedure SetMergeSource(const Value: TDataSource);
  protected
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataUpdate(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    function EditCanModify: Boolean; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Merge; overload; virtual;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MergeSource: TDataSource read GetMergeSource write SetMergeSource;
  end;


implementation

uses
  Graphics, SysUtils;

{ TDBAdvRichEditor }

procedure TDBAdvRichEditor.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
        Clear;
    end
    else
      Clear;
  end;
end;

constructor TDBAdvRichEditor.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  FDBUpdate := False;
end;

procedure TDBAdvRichEditor.DataChange(Sender: TObject);
var
  Stream: TStream;
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if Assigned(FDataLink.Field) and not FDBUpdate then
  begin
    // load from blob
    Stream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    try
      Clear;
      LoadFromStream(Stream);
      Refresh;
    finally
      Stream.Free;
    end;
    Refresh;
  end;
end;

procedure TDBAdvRichEditor.DataUpdate(Sender: TObject);
var
  Stream: TStream;
begin
  if Assigned(FDataLink.Field) then
  begin
    FDBUpdate := true;
    Stream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
      FDBUpdate := false;
    end;
  end;
end;

destructor TDBAdvRichEditor.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TDBAdvRichEditor.EditCanModify: Boolean;
begin
  Result := True;

  if not Assigned(FDataLink.DataSet) then
    Exit;

  FDBUpdate := True;

  try
    if not (FDataLink.DataSet.State in [dsEdit,dsInsert]) then
    begin
      Result := FDataLink.Edit;
//      DataUpdate(Self);
      FDataLink.Modified;
    end
    else
    begin
      Result := True;
//      DataUpdate(Self);
      FDataLink.Modified;
    end;

  finally
    FDBUpdate := False;
  end;
end;

function TDBAdvRichEditor.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvRichEditor.GetDataSource: TDataSource;
begin
  Result := nil;
  if Assigned(FDataLink) then
    Result := FDataLink.DataSource
end;

function TDBAdvRichEditor.GetMergeSource: TDataSource;
begin
  Result := FMergeSource;
end;

procedure TDBAdvRichEditor.Merge;
var
  i: integer;
  el: TREElement;
  gr: TPictureElement;
  fld: TField;
  ms: TMemoryStream;
begin
  if not Assigned(FMergeSource) then
    raise Exception.Create('MergeSource not assigned');

  if not Assigned(FMergeSource.DataSet) then
    raise Exception.Create('MergeSource dataset not assigned');

  MergeContext.Clear;
  CloneContext(Context, MergeContext);

  for i := 0 to Context.Content.Count - 1 do
  begin
    el := Context.Content[i];

    if el.MergeRef <> '' then
    begin
      fld := FMergeSource.DataSet.FieldByName(el.MergeRef);

      if Assigned(fld) then
      begin
        if fld.DataType in [ftGraphic, ftBlob] then
        begin
          ms := TMemoryStream.Create;
          try
            (fld as TBlobField).SaveToStream(ms);
            ms.Position := 0;
            gr := TPictureElement.Create;
            gr.Picture.LoadFromStream(ms);
            Context.Content.Insert(ElementIndex(el), gr);
          finally
            ms.Free;
          end;
        end
        else
          el.Text := fld.Text;

        el.MergeRef := '';
      end;
    end;
  end;

  Refresh;
end;

procedure TDBAdvRichEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = DataSource) and (AOperation = opRemove) then
    DataSource := nil;

  if (AComponent = MergeSource) and (AOperation = opRemove) then
    MergeSource := nil;
end;

procedure TDBAdvRichEditor.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvRichEditor.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvRichEditor.SetMergeSource(const Value: TDataSource);
begin
  FMergeSource := Value;
end;

end.
