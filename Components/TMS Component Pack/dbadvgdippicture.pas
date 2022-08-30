{***************************************************************************}
{ TDBAdvGDIPPicture component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
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

unit DBAdvGDIPPicture;

interface

uses
  Windows, Classes, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, AdvGDIPicture;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvGDIPPicture = class(TAdvGDIPPicture)
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
    procedure PictureChange; override;
    procedure DoPictureSelected(FileName: string); override;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True;
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

{ TDBAdvGDIPPicture }

procedure TDBAdvGDIPPicture.ActiveChange(Sender: TObject);
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

function TDBAdvGDIPPicture.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.CMExit(var Message: TCMExit);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

//------------------------------------------------------------------------------

constructor TDBAdvGDIPPicture.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  //FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange := ActiveChange;
  ReadOnly := True;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.DataChange(Sender: TObject);
begin
  if not CheckDataSet then
    Exit;

  FPictureLoaded := False;
  Picture.Assign(nil);
  FPictureLoaded := False;
  LoadPicture;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.UpdateData(Sender: TObject);
begin
  if not CheckDataSet or not FDataLink.Editing then
    Exit;

  if not Picture.Empty then FDataLink.Field.Assign(Picture)
  else FDataLink.Field.Clear;
end;

//------------------------------------------------------------------------------

destructor TDBAdvGDIPPicture.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvGDIPPicture.DoPictureSelected(FileName: string);
var
  ms: TMemoryStream;
  bf: TBlobField;
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

function TDBAdvGDIPPicture.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvGDIPPicture.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

function TDBAdvGDIPPicture.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TDBAdvGDIPPicture.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TDBAdvGDIPPicture.GetField: TField;
begin
  Result := FDataLink.Field;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.LoadPicture;
var
  s: TStream;
  ms: TMemoryStream;
  blobf: TBlobField;
  d: int64;
begin
  if not FPictureLoaded then
  begin
    if (not Assigned(FDataLink.Field) or FDataLink.Editing) or FDataLink.Field.IsNull then
      Picture.Assign(nil)
    else if (FDataLink.Field.IsBlob) then
    begin
      blobf := FDataLink.Field as TBlobField;
      s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
      //P := Picture.OnChange;      // On change can be avoided if needed
      try
        s.Position := 0;

        d := 0;

        if s.Size > 8 then
          s.Read(d,8);

        // TPicture class identifier
        if (d and $FFFFFFFF = $01000001) and (s.Size > 8) then
        begin
          ms := TMemoryStream.Create;
          ms.CopyFrom(s, s.Size - 8);
          ms.Position := 0;
          Picture.LoadFromStream(ms);
          ms.Free;
        end
        else
        begin
          s.Position := 0;
          Picture.LoadFromStream(s);
        end;
      finally
        s.Free;
      end;
      //Picture.Assign(FDataLink.Field);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.Notification(AComponent: TComponent;
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

procedure TDBAdvGDIPPicture.PictureChange;
begin
  inherited;

  if CheckDataSet and FPictureLoaded and FDataLink.CanModify and FDataLink.Edit then
    FDataLink.Modified;
  FPictureLoaded := True;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGDIPPicture.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

function TDBAdvGDIPPicture.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------


end.
