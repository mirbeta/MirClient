// ---------------------------------------------------------------------------
#ifndef DBUriStreamProviderH
#define DBUriStreamProviderH
// ---------------------------------------------------------------------------

#include "Classes.hpp"
#include "DBClient.hpp"
#include "SysUtils.hpp"
#include "DB.hpp"
#include "dxRichEdit.Utils.UriStreamService.hpp"
#include "dxRichEdit.Control.hpp"
#include "ComObj.hpp"

class TdxDBUriStreamProvider : public TCppInterfacedObject<IdxUriStreamProvider>
{
private:
	TClientDataSet* FDataSet;
	UnicodeString FField, FKey, FPrefix;

	void GetCloneData(TClientDataSet* ADataSet);
	void LoadData(TClientDataSet* ADataSet);
protected:
	virtual TStream* __fastcall GetStream(const UnicodeString AUrl);
	bool __fastcall IsAsyncLoadingSupported(const UnicodeString AUrl);
public:
	__fastcall TdxDBUriStreamProvider(TClientDataSet* ADataSet,
		UnicodeString AKey, UnicodeString AField, UnicodeString APrefix);
	__fastcall ~TdxDBUriStreamProvider();
};

void RegistrationDBUriStreamProvider(TdxRichEditControl* ARichEdit, TClientDataSet* ADataSet,
	UnicodeString AKey, UnicodeString AField, UnicodeString APrefix);
#endif
