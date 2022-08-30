unit uMD5;

interface
uses SysUtils, OverbyteIcsMD5;

function MD5String(const AContent: String): String;
function MD5File(const AFileName: String): String;

implementation

function MD5String(const AContent: String): String;
begin
  Result  :=  OverbyteIcsMD5.StrMD5(AContent);
end;

function MD5File(const AFileName: String): String;
begin
  Result  :=  OverbyteIcsMD5.FileMD5(AFileName, fmOpenRead or fmShareDenyNone);
end;

end.
