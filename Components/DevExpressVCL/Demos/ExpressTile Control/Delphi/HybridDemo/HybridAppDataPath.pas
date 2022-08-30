unit HybridAppDataPath;

interface

function GetDataPath: string;

implementation

function GetDataPath: string;
begin
  Result := '..\..\Data\';
end;

end.
