DROP VIEW IF EXISTS article_list_view;
CREATE VIEW article_list_view AS
  SELECT article.id,
         article.slug,
         article.title,
         article.image,
         article.synopsis,
         article.published,
         author.full_name AS author_name,
         array_remove (array_agg (article_tag.tag), NULL) AS tags
  FROM article
  LEFT OUTER JOIN "user" AS author ON author.id = article.author
  LEFT OUTER JOIN article_tag ON article_tag.article = article.id
  WHERE article.published IS NOT NULL
  GROUP BY article.id, author.id
  ORDER BY article.published DESC;
