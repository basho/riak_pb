/*
 * Copyright 2014 Brian Roach <roach at basho dot com>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.basho.riak.protobuf.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Brian Roach <roach at basho dot com>
 */
public class TemplateController
{
    public Map<String, Object> getProperties() throws IOException
    {
        HashMap<String, Object> result = new HashMap<String, Object>();
        
        File f = new File("src/riak_pb_messages.csv");
        InputStream stream = 
            new FileInputStream(f);
        
        BufferedReader in = new BufferedReader(new InputStreamReader(stream));
    
        String line;
        StringBuilder sb = new StringBuilder();
        String prefix = "public static final byte MSG_";
        String cast = "(byte)";
        
        while ((line = in.readLine()) != null)
        {
            String[] csv = line.split(",");
            
            String constName = csv[1].replaceFirst("^Rpb", "");
            
            sb.append(prefix).append(constName).append(" = ");
            
            int code = Integer.valueOf(csv[0]);
            if (code > 127)
            {
                sb.append(cast);
            }
            
            sb.append(csv[0]).append(";\n");
            
        }
        
        result.put("packageName", "com.basho.riak.protobuf");
        result.put("codes", sb.toString());
        return result;
    }
}
