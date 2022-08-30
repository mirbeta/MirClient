{***************************************************************************}
{ TAdvDBLabel component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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
unit DBAdvLabel;

interface

uses
  AdvLabel, DB, DBCtrls, Windows, Classes, Types, Controls;

type
  TDBAdvLabel = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}

  TDBAdvLabel = class(TAdvLabel)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
  protected
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property Datasource:  TDataSource read GetDataSource write SetDataSource;
  end;

implementation




{ TDBAdvLabel }

constructor TDBAdvLabel.Create(AOwner: TComponent);
begin
  inherited;

  FDataLink := TFieldDataLink.Create;
  ControlStyle := ControlStyle + [csReplicatable];

  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;

end;

procedure TDBAdvLabel.DataChange(Sender: TObject);
begin
  try
    if Assigned(FDataLink.Field) then
  	  if FDataLink.Field.IsNull then
	      Text := ''
      else
      begin
        if (FDataLink.Field.DataType in [ftBlob, ftMemo, ftWideMemo]) then
          Text := FDataLink.Field.AsString
        else
          Text := FDataLink.Field.DisplayText;
      end;
  except
  end;
end;

procedure TDBAdvLabel.DataUpdate(Sender: TObject);
begin

end;

destructor TDBAdvLabel.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TDBAdvLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvLabel.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

end.
