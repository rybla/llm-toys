import OpenAI from "openai";

// ollama
const openai = new OpenAI({
  apiKey: "ollama",
  baseURL: "http://localhost:11434/v1",
});

const response_format = {
  "type": "json_schema",
  "json_schema": {
    "strict": true,
    "schema": {
      "type": "object",
      "required": [
        "value"
      ],
      "properties": {
        "value": {
          "anyOf": [
            {
              "value": {
                "type": "string"
              },
              "tag": {
                "type": "string",
                "const": "String"
              }
            },
            {
              "value": {
                "type": "integer"
              },
              "tag": {
                "type": "string",
                "const": "Int"
              }
            }
          ]
        }
      },
      "additionalProperties": false
    },
    "name": "StringOrInt"
  }
}

const response = await openai.chat.completions.create({
  model: "command-r7b:latest",
  messages: [
    {
      role: "user",
      content: "Generate either a string or an integer",
    }
  ],
  response_format: {
    "type": "json_schema",
    "json_schema": {
      "name": "StringOrInt",
      "strict": true,
      "schema": {
        "type": "object",
        "required": [
          "value"
        ],
        "properties": {
          "value": {
            "anyOf": [
              {
                "type": "string",
                "tag": {
                  "type": "string",
                  "const": "String"
                }
              },
              {
                "type": "integer",
                "tag": {
                  "type": "string",
                  "const": "Int"
                }
              }
            ]
          }
        },
        "additionalProperties": false
      },
    }
  }
})

console.log(response.choices[0].message);