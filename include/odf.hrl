-type document_type() :: text 
                       | spreadsheet 
                       | drawing 
                       | presentation 
                       | chart 
                       | image 
                       | formula
                       | database.

-record(odf_document, 
        {
         path :: file:filename_all() | undefined,
         data  :: binary() | undefined,
         files = [] :: [{file:filename_all(), binary()}],
         format = zip :: zip | xml,
         type = text :: document_type(),
         files_sup :: pid(),
         document_sup :: pid()
        }).

-record(odf_file,
        {
         name :: string(),
         data :: iodata(),
         xml :: xmerl:document(),
         modified=false :: boolean()
        }).
