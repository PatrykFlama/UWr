using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Json;
using System.Web;

namespace Zad4.Google
{
    public class GoogleProfileAPI
    {
        public string email { get; set; }
        public string given_name { get; set; }
        public string family_name { get; set; }

        private static DataContractJsonSerializer jsonSerializer =
            new DataContractJsonSerializer(typeof(GoogleProfileAPI));

        public static GoogleProfileAPI Deserialize(Stream jsonStream)
        {
            try
            {
                if(jsonStream == null)
                {
                    throw new ArgumentNullException("jsonStream");
                }

                return (GoogleProfileAPI)jsonSerializer.ReadObject(jsonStream);
            }
            catch(Exception ex)
            {
                return new GoogleProfileAPI();
            }
        }
    }


}