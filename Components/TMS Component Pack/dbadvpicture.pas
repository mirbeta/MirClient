{***************************************************************************}
{ TDBAdvPicture component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2011                                        }
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

{$I TMSDEFS.INC}

unit DBAdvPicture;

interface

uses
  Windows, Classes, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, AdvPicture;

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvPicture = class(TAdvPicture)
  private
    FDataLink: TFieldDataLink;
    FPictureLoaded: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CheckDataSet: Boolean;
    procedure LoadPicture;
    procedure BlobFieldToStream(DBField: TBlobField);
    procedure PictureChange; override;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True;
    procedure DoPictureSelected(FileName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


implementation


type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//------------------------------------------------------------------------------

{ TDBAdvPicture }

procedure TDBAdvPicture.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) and FDataLink.Active then
    begin
      FPictureLoaded := False;
      LoadPicture;
    end
    else
    begin
      FPictureLoaded := False;
      Picture.Assign(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.CMExit(var Message: TCMExit);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

//------------------------------------------------------------------------------

constructor TDBAdvPicture.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  ReadOnly := True;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.DataChange(Sender: TObject);
begin
  if not CheckDataSet then
    Exit;

  FPictureLoaded := False;
  Picture.Assign(nil);
  FPictureLoaded := False;
  LoadPicture;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.UpdateData(Sender: TObject);
begin
  if not CheckDataSet or not FDataLink.Editing then
    Exit;

  if not Picture.Empty then
    FDataLink.Field.Assign(Picture)
  else
    FDataLink.Field.Clear;
end;

//------------------------------------------------------------------------------

destructor TDBAdvPicture.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvPicture.DoPictureSelected(FileName: string);
var
  bf: TBlobField;
  ms: TMemoryStream;
begin
  inherited;
  if not CheckDataSet then
    Exit;

  if FDataLink.Field is TBlobField then
  begin
    ms := TMemoryStream.Create;
    Picture.SaveToStream(ms);

    DataSource.DataSet.Edit;

    bf := TBlobField(FDatalink.Field);
    try
      ms.Position := 0;
      bf.LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.GetField: TField;
begin
  Result := FDataLink.Field;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.BlobFieldToStream(DBField: TBlobField);
var
  s: TMemoryStream;
  sig: word;
  b: byte;
  oleoffset: integer;
  i: Integer;
  ms: TMemoryStream;

begin
  s := TMemoryStream.Create;
  try
    DBField.SaveToStream(S);
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
                oleoffset := i;
                Break;
              end;
            end;
          end;

          S.Position := oleoffset;
          ms := TMemoryStream.Create;
          ms.CopyFrom(s, s.Size - oleoffset);
          ms.Position := 0;
          try
            Picture.LoadFromStream(ms);
          finally
            ms.Free;
          end;
        end;
        $4947: //gif signature
        begin
          s.Position := 0;
          Picture.LoadFromStream(s);
        end;
        $D8FF: //jpeg signature
        begin
          S.Position := 0;
          Picture.LoadFromStream(s);
        end
        else
          begin
            Picture.LoadFromStream(s);
          end;
        end;
      end;
  finally
    FreeAndNil(S);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.LoadPicture;
var
  blobf: TBlobField;
begin
  if not FPictureLoaded then
  begin
    if (not Assigned(FDataLink.Field) or FDataLink.Editing) or FDataLink.Field.IsNull then
      Picture.Assign(nil)
    else
    if (FDataLink.Field.IsBlob) then
    begin
      blobf := FDataLink.Field as TBlobField;
      BlobFieldToStream(blobf);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.PictureChange;
begin
  inherited;

  if CheckDataSet and FPictureLoaded and FDataLink.CanModify and FDataLink.Edit then
    FDataLink.Modified;
  FPictureLoaded := True;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvPicture.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

function TDBAdvPicture.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------


end.
